{-# LANGUAGE CPP #-}
module GhcUtil (withGhc) where

import           Control.Exception
import           GHC.Paths (libdir)
import           GHC hiding (flags)
import           DynFlags (dopt_set)
import           Panic (ghcError)

import           MonadUtils (liftIO)
import           System.Exit (exitFailure)

#if __GLASGOW_HASKELL__ < 702
import           StaticFlags (v_opt_C_ready)
import           Data.IORef (writeIORef)
#else
import           StaticFlags (saveStaticFlagGlobals, restoreStaticFlagGlobals)
#endif


-- | Save static flag globals, run action, and restore them.
bracketStaticFlags :: IO a -> IO a
#if __GLASGOW_HASKELL__ < 702
-- GHC < 7.2 does not provide saveStaticFlagGlobals/restoreStaticFlagGlobals,
-- so we need to modifying v_opt_C_ready directly
bracketStaticFlags action = action `finally` writeIORef v_opt_C_ready False
#else
bracketStaticFlags action = bracket saveStaticFlagGlobals restoreStaticFlagGlobals (const action)
#endif

-- Catch GHC source errors, print them and exit.
handleSrcErrors :: Ghc a -> Ghc a
handleSrcErrors action' = flip handleSourceError action' $ \err -> do
#if __GLASGOW_HASKELL__ < 702
  printExceptionAndWarnings err
#else
  printException err
#endif
  liftIO exitFailure

-- | Run a GHC action in Haddock mode
withGhc :: [String] -> ([String] -> Ghc a) -> IO a
withGhc flags action = bracketStaticFlags $ do
  flags_ <- handleStaticFlags flags

  runGhc (Just libdir) $ do
    handleDynamicFlags flags_ >>= handleSrcErrors . action

handleStaticFlags :: [String] -> IO [Located String]
handleStaticFlags flags = fst `fmap` parseStaticFlags (map noLoc flags)

handleDynamicFlags :: GhcMonad m => [Located String] -> m [String]
handleDynamicFlags flags = do
  (dynflags, locSrcs, _) <- (setHaddockMode `fmap` getSessionDynFlags) >>= flip parseDynamicFlags flags
  _ <- setSessionDynFlags dynflags

  -- We basically do the same thing as `ghc/Main.hs` to distinguish
  -- "unrecognised flags" from source files.
  let srcs = map unLoc locSrcs
      unknown_opts = [ f | f@('-':_) <- srcs ]
  case unknown_opts of
    opt : _ -> ghcError (UsageError ("unrecognized option `"++ opt ++ "'"))
    _       -> return srcs

setHaddockMode :: DynFlags -> DynFlags
setHaddockMode dynflags = (dopt_set dynflags Opt_Haddock) {
      hscTarget = HscNothing
    , ghcMode   = CompManager
    , ghcLink   = NoLink
    }
