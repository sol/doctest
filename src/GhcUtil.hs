{-# LANGUAGE CPP #-}
module GhcUtil (withGhc) where

import            Control.Exception

import            GHC.Paths (libdir)
import            GHC hiding (flags)
import            DynFlags (dopt_set)

#if __GLASGOW_HASKELL__ < 702
import            StaticFlags (v_opt_C_ready)
import            Data.IORef (writeIORef)
#else
import            StaticFlags (saveStaticFlagGlobals, restoreStaticFlagGlobals)
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

-- | Run a GHC action in Haddock mode
withGhc :: [String] -> Ghc a -> IO a
withGhc flags action = bracketStaticFlags $ do
  flags_ <- handleStaticFlags flags
  runGhc (Just libdir) $ do
    handleDynamicFlags flags_
    action

handleStaticFlags :: [String] -> IO [Located String]
handleStaticFlags flags = fst `fmap` parseStaticFlags (map noLoc flags)

handleDynamicFlags :: GhcMonad m => [Located String] -> m ()
handleDynamicFlags flags = do
  (dynflags, rest, _) <- (setHaddockMode `fmap` getSessionDynFlags) >>= flip parseDynamicFlags flags
  case rest of
    x : _ -> error ("Unrecognized GHC option: " ++ unLoc x)
    _     -> setSessionDynFlags dynflags >> return ()

setHaddockMode :: DynFlags -> DynFlags
setHaddockMode dynflags = (dopt_set dynflags Opt_Haddock) {
      hscTarget = HscNothing
    , ghcMode   = CompManager
    , ghcLink   = NoLink
    }
