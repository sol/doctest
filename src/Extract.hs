{-# LANGUAGE CPP #-}
module Extract (Module(..), extract) where

import           Imports hiding (mod, concat)
import           Data.List (partition, isSuffixOf)

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

import           System.Posix.Internals (c_getpid)

import           GhcUtil (withGhc)
import           Location hiding (unLoc)

import           Util (convertDosLineEndings)
import           PackageDBs (getPackageDBArgs)

#if __GLASGOW_HASKELL__ < 900
import           DynamicLoading (initializePlugins)
#else
import           GHC.Runtime.Loader (initializePlugins)
#endif

#if __GLASGOW_HASKELL__ >= 901
import           GHC.Unit.Module.Graph
#endif

-- | A wrapper around `SomeException`, to allow for a custom `Show` instance.
newtype ExtractError = ExtractError SomeException
#if __GLASGOW_HASKELL__ < 912
  deriving Typeable
#endif

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
} deriving (Eq, Show, Functor)

instance NFData a => NFData (Module a) where
  rnf (Module name setup content) = name `deepseq` setup `deepseq` content `deepseq` ()

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

  let sortedMods =
#if __GLASGOW_HASKELL__ >= 914
                   mapMaybe maybeModSummaryFromModuleNodeInfo $
#endif
                   flattenSCCs
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

    -- Since GHC 8.6, plugins are initialized on a per module basis
    loadModPlugins modsum = do
      _ <- setSessionDynFlags (GHC.ms_hspp_opts modsum)
      hsc_env <- getSession

# if __GLASGOW_HASKELL__ >= 902
      hsc_env' <- liftIO (initializePlugins hsc_env)
      setSession hsc_env'
      return modsum
# else
      dynflags' <- liftIO (initializePlugins hsc_env (GHC.ms_hspp_opts modsum))
      return $ modsum { ms_hspp_opts = dynflags' }
# endif

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -> IO [Module (Located String)]
extract args = do
  packageDBArgs <- getPackageDBArgs
  let
    args' = args ++
#if __GLASGOW_HASKELL__ >= 810
      -- `ghci` ignores unused packages in certain situation.  This ensures
      -- that we don't fail in situations where `ghci` would not.
      "-Wno-unused-packages" :
#endif
      packageDBArgs

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

#if __GLASGOW_HASKELL__ >= 914
maybeModSummaryFromModuleNodeInfo :: ModuleNodeInfo -> Maybe ModSummary
maybeModSummaryFromModuleNodeInfo i = case i of
  ModuleNodeCompile summary -> Just summary
  ModuleNodeFixed _ _ -> Nothing
#endif

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
#if __GLASGOW_HASKELL__ >= 906
    header  = [(Nothing, hsDocString <$> x) | Just x <- [hsmodHaddockModHeader (hsmodExt source)]]
#elif __GLASGOW_HASKELL__ >= 904
    header  = [(Nothing, hsDocString <$> x) | Just x <- [hsmodHaddockModHeader (source)]]
#else
    header  = [(Nothing, x) | Just x <- [hsmodHaddockModHeader source]]
#endif
    exports :: [(Maybe String, LHsDocString)]
#if __GLASGOW_HASKELL__ >= 904
    exports = [ (Nothing, L (locA loc) (hsDocString (unLoc doc)))
#else
    exports = [ (Nothing, L (locA loc) doc)
#endif
              | L loc (IEDoc _ doc) <- maybe [] unLoc (hsmodExports source)
              ]
    decls :: [(Maybe String, LHsDocString)]
    decls   = extractDocStrings (hsmodDecls source)

-- | Extract all docstrings from given value.
extractDocStrings :: Data a => a -> [(Maybe String, LHsDocString)]
extractDocStrings d =
#if __GLASGOW_HASKELL__ >= 904
  let
    docStrs = extractAll extractDocDocString d
    docStrNames = catMaybes $ extractAll extractDocName d
  in
    flip fmap docStrs $ \docStr -> (lookup (getLoc docStr) docStrNames, docStr)
  where
    extractAll z = everything (++) (mkQ [] ((:[]) . z))

    extractDocDocString :: LHsDoc GhcPs -> LHsDocString
    extractDocDocString = fmap hsDocString

    extractDocName :: DocDecl GhcPs -> Maybe (SrcSpan, String)
    extractDocName docDecl = case docDecl of
      DocCommentNamed name y ->
        Just (getLoc y, name)
      _ ->
        Nothing
#else
  everythingBut (++) (([], False) `mkQ` fromLHsDecl
  `extQ` fromLDocDecl
  `extQ` fromLHsDocString
  ) d
  where
    fromLHsDecl :: Selector (LHsDecl GhcPs)
    fromLHsDecl (L loc decl) = case decl of

      -- Top-level documentation has to be treated separately, because it has
      -- no location information attached.  The location information is
      -- attached to HsDecl instead.
      DocD _ x
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

    fromDocDecl :: SrcSpan -> DocDecl -> (Maybe String, LHsDocString)
    fromDocDecl loc x = case x of
      DocCommentNamed name doc -> (Just name, L loc doc)
      _                        -> (Nothing, L loc $ docDeclDoc x)

type Selector a = a -> ([(Maybe String, LHsDocString)], Bool)

-- | Collect given value and descend into subtree.
select :: a -> ([a], Bool)
select x = ([x], False)
#endif

#if __GLASGOW_HASKELL__ < 901
locA :: SrcSpan -> SrcSpan
locA = id
#endif
