{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor #-}
module Extract (Module(..), extract) where

import           Prelude hiding (mod, concat)
import           Control.Monad
#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Exception
import           Data.List (partition, isSuffixOf)
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import           Data.Foldable (concat)
#endif

import           Control.DeepSeq (deepseq, NFData(rnf))
import           Data.Generics

#if __GLASGOW_HASKELL__ < 707
import           GHC hiding (flags, Module, Located)
import           MonadUtils (liftIO, MonadIO)
#else
import           GHC hiding (Module, Located)
import           DynFlags
import           MonadUtils (liftIO)
#endif
import           Exception (ExceptionMonad)
import           System.Directory
import           System.FilePath

#if __GLASGOW_HASKELL__ < 710
import           NameSet (NameSet)
import           Coercion (Coercion)
#endif

import           FastString (unpackFS)
import           Digraph (flattenSCCs)

import           System.Posix.Internals (c_getpid)

import           GhcUtil (withGhc)
import           Location hiding (unLoc)

import           Util (convertDosLineEndings)
import           PackageDBs (getPackageDBArgs)

-- | A wrapper around `SomeException`, to allow for a custom `Show` instance.
newtype ExtractError = ExtractError SomeException
  deriving Typeable

instance Show ExtractError where
  show (ExtractError e) =
    unlines [
        "Ouch! Hit an error thunk in GHC's AST while extracting documentation."
      , ""
      , "    " ++ msg
      , ""
      , "This is most likely a bug in doctest."
      , ""
      , "Please report it here: https://github.com/sol/doctest/issues/new"
      ]
    where
      msg = case fromException e of
        Just (Panic s) -> "GHC panic: " ++ s
        _              -> show e

instance Exception ExtractError

-- | Documentation for a module grouped together with the modules name.
data Module a = Module {
  moduleName    :: String
, moduleSetup   :: Maybe a
, moduleContent :: [a]
} deriving (Eq, Functor)

instance NFData a => NFData (Module a) where
  rnf (Module name setup content) = name `deepseq` setup `deepseq` content `deepseq` ()

#if __GLASGOW_HASKELL__ < 803
type GhcPs = RdrName

needsTemplateHaskellOrQQ :: ModuleGraph -> Bool
needsTemplateHaskellOrQQ = needsTemplateHaskell

mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG = map
#endif

-- | Parse a list of modules.
parse :: [String] -> IO [TypecheckedModule]
parse args = withGhc args $ \modules_ -> withTempOutputDir $ do

  -- ignore additional object files
  let modules = filter (not . isSuffixOf ".o") modules_

  mapM (`guessTarget` Nothing) modules >>= setTargets
  mods <- depanal [] False

  mods' <- if needsTemplateHaskellOrQQ mods then enableCompilation mods else return mods

  let sortedMods = flattenSCCs (topSortModuleGraph False mods' Nothing)
  reverse <$> mapM (parseModule >=> typecheckModule >=> loadModule) sortedMods
  where
    -- copied from Haddock/Interface.hs
    enableCompilation :: ModuleGraph -> Ghc ModuleGraph
    enableCompilation modGraph = do
#if __GLASGOW_HASKELL__ < 707
      let enableComp d = d { hscTarget = defaultObjectTarget }
#else
      let enableComp d = let platform = targetPlatform d
                         in d { hscTarget = defaultObjectTarget platform }
#endif
      modifySessionDynFlags enableComp
      -- We need to update the DynFlags of the ModSummaries as well.
      let upd m = m { ms_hspp_opts = enableComp (ms_hspp_opts m) }
      let modGraph' = mapMG upd modGraph
      return modGraph'

    -- copied from Haddock/GhcUtils.hs
    modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
    modifySessionDynFlags f = do
      dflags <- getSessionDynFlags
#if __GLASGOW_HASKELL__ < 707
      _ <- setSessionDynFlags (f dflags)
#else
      -- GHCi 7.7 now uses dynamic linking.
      let dflags' = case lookup "GHC Dynamic" (compilerInfo dflags) of
            Just "YES" -> gopt_set dflags Opt_BuildDynamicToo
            _          -> dflags
      _ <- setSessionDynFlags (f dflags')
#endif
      return ()

    withTempOutputDir :: Ghc a -> Ghc a
    withTempOutputDir action = do
      tmp <- liftIO getTemporaryDirectory
      x   <- liftIO c_getpid
      let dir = tmp </> ".doctest-" ++ show x
      modifySessionDynFlags (setOutputDir dir)
      gbracket_
        (liftIO $ createDirectory dir)
        (liftIO $ removeDirectoryRecursive dir)
        action

    -- | A variant of 'gbracket' where the return value from the first computation
    -- is not required.
    gbracket_ :: ExceptionMonad m => m a -> m b -> m c -> m c
    gbracket_ before_ after thing = gbracket before_ (const after) (const thing)

    setOutputDir f d = d {
        objectDir  = Just f
      , hiDir      = Just f
      , stubDir    = Just f
      , includePaths = f : includePaths d
      }

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -> IO [Module (Located String)]
extract args = do
  packageDBArgs <- getPackageDBArgs
  let args'  = args ++ packageDBArgs
  mods <- parse args'
  let docs = map (fmap (fmap convertDosLineEndings) . extractFromModule . tm_parsed_module) mods

  (docs `deepseq` return docs) `catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      Handler (\e -> throw (e :: AsyncException))
    , Handler (throwIO . ExtractError)
    ]

-- | Extract all docstrings from given module and attach the modules name.
extractFromModule :: ParsedModule -> Module (Located String)
extractFromModule m = Module name (listToMaybe $ map snd setup) (map snd docs)
  where
    isSetup = (== Just "setup") . fst
    (setup, docs) = partition isSetup (docStringsFromModule m)
    name = (moduleNameString . GHC.moduleName . ms_mod . pm_mod_summary) m

-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedModule -> [(Maybe String, Located String)]
docStringsFromModule mod = map (fmap (toLocated . fmap unpackDocString)) docs
  where
    source   = (unLoc . pm_parsed_source) mod

    -- we use dlist-style concatenation here
    docs     = header ++ exports ++ decls

    -- We process header, exports and declarations separately instead of
    -- traversing the whole source in a generic way, to ensure that we get
    -- everything in source order.
    header  = [(Nothing, x) | Just x <- [hsmodHaddockModHeader source]]
#if __GLASGOW_HASKELL__ < 710
    exports = [(Nothing, L loc doc) | L loc (IEDoc doc) <- concat (hsmodExports source)]
#else
    exports = [(Nothing, L loc doc) | L loc (IEDoc doc) <- maybe [] unLoc (hsmodExports source)]
#endif
    decls   = extractDocStrings (hsmodDecls source)

type Selector a = a -> ([(Maybe String, LHsDocString)], Bool)

#if __GLASGOW_HASKELL__ < 710
-- | Ignore a subtree.
ignore :: Selector a
ignore = const ([], True)
#endif

-- | Collect given value and descend into subtree.
select :: a -> ([a], Bool)
select x = ([x], False)

-- | Extract all docstrings from given value.
extractDocStrings :: Data a => a -> [(Maybe String, LHsDocString)]
extractDocStrings = everythingBut (++) (([], False) `mkQ` fromLHsDecl
  `extQ` fromLDocDecl
  `extQ` fromLHsDocString
#if __GLASGOW_HASKELL__ < 710
  `extQ` (ignore :: Selector NameSet)
  `extQ` (ignore :: Selector PostTcKind)

  -- HsExpr never contains any documentation, but it may contain error thunks.
  --
  -- Problematic are (non comprehensive):
  --
  --  * parallel list comprehensions
  --  * infix operators
  --
  `extQ` (ignore :: Selector (HsExpr RdrName))

  -- undefined before type checking
  `extQ` (ignore :: Selector Coercion)

#if __GLASGOW_HASKELL__ >= 706
  -- hswb_kvs and hswb_tvs may be error thunks
  `extQ` (ignore :: Selector (HsWithBndrs [LHsType RdrName]))
  `extQ` (ignore :: Selector (HsWithBndrs [LHsType Name]))
  `extQ` (ignore :: Selector (HsWithBndrs (LHsType RdrName)))
  `extQ` (ignore :: Selector (HsWithBndrs (LHsType Name)))
#endif

#endif
  )
  where
    fromLHsDecl :: Selector (LHsDecl GhcPs)
    fromLHsDecl (L loc decl) = case decl of

      -- Top-level documentation has to be treated separately, because it has
      -- no location information attached.  The location information is
      -- attached to HsDecl instead.
      DocD x -> select (fromDocDecl loc x)

      _ -> (extractDocStrings decl, True)

    fromLDocDecl :: Selector LDocDecl
    fromLDocDecl (L loc x) = select (fromDocDecl loc x)

    fromLHsDocString :: Selector LHsDocString
    fromLHsDocString x = select (Nothing, x)

    fromDocDecl :: SrcSpan -> DocDecl -> (Maybe String, LHsDocString)
    fromDocDecl loc x = case x of
      DocCommentNamed name doc -> (Just name, L loc doc)
      _                        -> (Nothing, L loc $ docDeclDoc x)

-- | Convert a docstring to a plain string.
unpackDocString :: HsDocString -> String
unpackDocString (HsDocString s) = unpackFS s
