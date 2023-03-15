{-# LANGUAGE CPP #-}
module GhcUtil (withGhc) where

import           GHC.Paths (libdir)
import           GHC
#if __GLASGOW_HASKELL__ < 900
import           DynFlags (gopt_set)
#else
import           GHC.Driver.Session (gopt_set)
#endif

#if __GLASGOW_HASKELL__ < 900
import           Panic (throwGhcException)
#else
import           GHC.Utils.Panic (throwGhcException)
#endif

#if __GLASGOW_HASKELL__ < 900
import           MonadUtils (liftIO)
#else
import           GHC.Utils.Monad (liftIO)
#endif

import           System.Exit (exitFailure)

#if __GLASGOW_HASKELL__ < 801
import           StaticFlags (discardStaticFlags)
#endif

-- Catch GHC source errors, print them and exit.
handleSrcErrors :: Ghc a -> Ghc a
handleSrcErrors action' = flip handleSourceError action' $ \err -> do
  printException err
  liftIO exitFailure

-- | Run a GHC action in Haddock mode
withGhc :: [String] -> ([String] -> Ghc a) -> IO a
withGhc flags action = do
  flags_ <- handleStaticFlags flags

  runGhc (Just libdir) $ do
    handleDynamicFlags flags_ >>= handleSrcErrors . action

handleStaticFlags :: [String] -> IO [Located String]
#if __GLASGOW_HASKELL__ < 801
handleStaticFlags flags = return $ map noLoc $ discardStaticFlags flags
#else
handleStaticFlags flags = return $ map noLoc $ flags
#endif

handleDynamicFlags :: GhcMonad m => [Located String] -> m [String]
handleDynamicFlags flags = do
#if __GLASGOW_HASKELL__ >= 901
  logger <- getLogger
  let parseDynamicFlags' = parseDynamicFlags logger
#else
  let parseDynamicFlags' = parseDynamicFlags
#endif
  (dynflags, locSrcs, _) <- (setHaddockMode `fmap` getSessionDynFlags) >>= (`parseDynamicFlags'` flags)
  _ <- setSessionDynFlags dynflags

  -- We basically do the same thing as `ghc/Main.hs` to distinguish
  -- "unrecognised flags" from source files.
  let srcs = map unLoc locSrcs
      unknown_opts = [ f | f@('-':_) <- srcs ]
  case unknown_opts of
    opt : _ -> throwGhcException (UsageError ("unrecognized option `"++ opt ++ "'"))
    _       -> return srcs

setHaddockMode :: DynFlags -> DynFlags
setHaddockMode dynflags = (gopt_set dynflags Opt_Haddock) {
#if __GLASGOW_HASKELL__ >= 906
      backend   = noBackend
#elif __GLASGOW_HASKELL__ >= 901
      backend   = NoBackend
#else
      hscTarget = HscNothing
#endif
    , ghcMode   = CompManager
    , ghcLink   = NoLink
    }
