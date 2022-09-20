{-# LANGUAGE CPP, DeriveDataTypeable, DeriveFunctor #-}
module Extract (Module(..), extract) where

import           Prelude hiding (mod, concat)
import           Control.Monad
import           Control.Exception
import           Data.List (partition, isSuffixOf)
import           Data.Maybe

import           Control.DeepSeq (deepseq, NFData(rnf))
import           Data.Generics

#if __GLASGOW_HASKELL__ < 900
import           GHC hiding (Module, Located)
import           DynFlags
import           MonadUtils (liftIO)
#else
import           GHC hiding (Module, Located)
import           GHC.Driver.Session
import           GHC.Utils.Monad (liftIO)
#endif

#if __GLASGOW_HASKELL__ < 900
import           Digraph (flattenSCCs)
import           Exception (ExceptionMonad)
#else
import           GHC.Data.Graph.Directed (flattenSCCs)
import           GHC.Utils.Exception (ExceptionMonad)
import           Control.Monad.Catch (generalBracket)
#endif

import           System.Directory
import           System.FilePath

#if __GLASGOW_HASKELL__ < 805
import           FastString (unpackFS)
#endif

import           System.Posix.Internals (c_getpid)

import           GhcUtil (withGhc)
import           Location hiding (unLoc)

import           Util (convertDosLineEndings)
import           PackageDBs (getPackageDBArgs)

#if __GLASGOW_HASKELL__ >= 806
#if __GLASGOW_HASKELL__ < 900
import           DynamicLoading (initializePlugins)
#else
import           GHC.Runtime.Loader (initializePlugins)
#endif
#endif

#if __GLASGOW_HASKELL__ >= 901
import           GHC.Unit.Module.Graph
#endif

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
#endif

#if __GLASGOW_HASKELL__ < 805
addQuoteInclude :: [String] -> [String] -> [String]
addQuoteInclude includes new = new ++ includes
#endif

-- | Parse a list of modules.
parse :: [String] -> IO [ParsedModule]
parse args = withGhc args $ \modules_ -> withTempOutputDir $ do

  -- ignore additional object files
  let modules = filter (not . isSuffixOf ".o") modules_

  setTargets =<< forM modules (\ m -> guessTarget m
#if __GLASGOW_HASKELL__ >= 903
                Nothing
#endif
                Nothing)
  mods <- depanal [] False

  let sortedMods = flattenSCCs
#if __GLASGOW_HASKELL__ >= 901
                     $ filterToposortToModules
#endif
                     $ topSortModuleGraph False mods Nothing
  reverse <$> mapM (loadModPlugins >=> parseModule) sortedMods
  where

    -- copied from Haddock/GhcUtils.hs
    modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
    modifySessionDynFlags f = do
      dflags <- getSessionDynFlags
      -- GHCi 7.7 now uses dynamic linking.
      let dflags' = case lookup "GHC Dynamic" (compilerInfo dflags) of
            Just "YES" -> gopt_set dflags Opt_BuildDynamicToo
            _          -> dflags
      _ <- setSessionDynFlags (f dflags')
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
#if __GLASGOW_HASKELL__ < 900
    gbracket_ before_ after thing = gbracket before_ (const after) (const thing)
#else
    gbracket_ before_ after thing = fst <$> generalBracket before_ (\ _ _ -> after) (const thing)
#endif

    setOutputDir f d = d {
        objectDir  = Just f
      , hiDir      = Just f
      , stubDir    = Just f
      , includePaths = addQuoteInclude (includePaths d) [f]
      }

#if __GLASGOW_HASKELL__ >= 806
    -- Since GHC 8.6, plugins are initialized on a per module basis
    loadModPlugins modsum = do
      _ <- setSessionDynFlags (GHC.ms_hspp_opts modsum)
      hsc_env <- getSession

# if __GLASGOW_HASKELL__ >= 902
      hsc_env' <- liftIO (initializePlugins hsc_env)
      setSession hsc_env'
      return $ modsum
# else
      dynflags' <- liftIO (initializePlugins hsc_env (GHC.ms_hspp_opts modsum))
      return $ modsum { ms_hspp_opts = dynflags' }
# endif
#else
    loadModPlugins = return
#endif

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -> IO [Module (Located String)]
extract args = do
  packageDBArgs <- getPackageDBArgs
  let args'  = args ++ packageDBArgs
  mods <- parse args'
  let docs = map (fmap (fmap convertDosLineEndings) . extractFromModule) mods

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

#if __GLASGOW_HASKELL__ >= 904
unpackHDS :: HsDocString -> String
unpackHDS = renderHsDocString
#endif

-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedModule -> [(Maybe String, Located String)]
docStringsFromModule mod = map (fmap (toLocated . fmap unpackHDS)) docs
  where
    source   = (unLoc . pm_parsed_source) mod

    -- we use dlist-style concatenation here
    docs :: [(Maybe String, LHsDocString)]
    docs     = header ++ exports ++ decls

    -- We process header, exports and declarations separately instead of
    -- traversing the whole source in a generic way, to ensure that we get
    -- everything in source order.
#if __GLASGOW_HASKELL__ >= 904
    header  = [(Nothing, hsDocString <$> x) | Just x <- [hsmodHaddockModHeader source]]
#else
    header  = [(Nothing, x) | Just x <- [hsmodHaddockModHeader source]]
#endif
    exports :: [(Maybe String, LHsDocString)]
#if __GLASGOW_HASKELL__ >= 904
    exports = [ (Nothing, L (locA loc) (hsDocString (unLoc doc)))
#else
    exports = [ (Nothing, L (locA loc) doc)
#endif
#if __GLASGOW_HASKELL__ < 805
              | L loc (IEDoc doc) <- maybe [] unLoc (hsmodExports source)
#else
              | L loc (IEDoc _ doc) <- maybe [] unLoc (hsmodExports source)
#endif
              ]
    decls :: [(Maybe String, LHsDocString)]
    decls   = extractDocStrings (hsmodDecls source)

type Selector a = a -> ([(Maybe String, LHsDocString)], Bool)

-- | Collect given value and descend into subtree.
select :: a -> ([a], Bool)
select x = ([x], False)

-- | Extract all docstrings from given value.
extractDocStrings :: Data a => a -> [(Maybe String, LHsDocString)]
extractDocStrings = everythingBut (++) (([], False) `mkQ` fromLHsDecl
  `extQ` fromLDocDecl
  `extQ` fromLHsDocString
  )
  where
    fromLHsDecl :: Selector (LHsDecl GhcPs)
    fromLHsDecl (L loc decl) = case decl of

      -- Top-level documentation has to be treated separately, because it has
      -- no location information attached.  The location information is
      -- attached to HsDecl instead.
#if __GLASGOW_HASKELL__ < 805
      DocD x
#else
      DocD _ x
#endif
               -> select (fromDocDecl (locA loc) x)

      _ -> (extractDocStrings decl, True)

    fromLDocDecl :: Selector
#if __GLASGOW_HASKELL__ >= 901
                             (LDocDecl GhcPs)
#else
                             LDocDecl
#endif
    fromLDocDecl (L loc x) = select (fromDocDecl (locA loc) x)

    fromLHsDocString :: Selector LHsDocString
    fromLHsDocString x = select (Nothing, x)

#if __GLASGOW_HASKELL__ >= 904
    fromDocDecl :: SrcSpan -> DocDecl GhcPs -> (Maybe String, LHsDocString)
    fromDocDecl loc x = case x of
      DocCommentNamed name doc -> (Just name, L loc (hsDocString (unLoc doc)))
      _                        -> (Nothing, L loc (hsDocString (unLoc (docDeclDoc x))))
#else
    fromDocDecl :: SrcSpan -> DocDecl -> (Maybe String, LHsDocString)
    fromDocDecl loc x = case x of
      DocCommentNamed name doc -> (Just name, L loc doc)
      _                        -> (Nothing, L loc $ docDeclDoc x)
#endif

#if __GLASGOW_HASKELL__ < 805
-- | Convert a docstring to a plain string.
unpackHDS :: HsDocString -> String
unpackHDS (HsDocString s) = unpackFS s
#endif

#if __GLASGOW_HASKELL__ < 901
locA :: SrcSpan -> SrcSpan
locA = id
#endif
