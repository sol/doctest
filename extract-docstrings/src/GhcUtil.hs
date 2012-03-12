module GhcUtil (withGhc) where

import GHC.Paths (libdir)
import GHC hiding (flags)
import Data.IORef (writeIORef)
import StaticFlags (v_opt_C_ready)
import DynFlags (dopt_set)

-- | Run a GHC action in Haddock mode
withGhc :: [String] -> Ghc a -> IO a
withGhc flags action = do
  flags_ <- handleStaticFlags flags
  runGhc (Just libdir) $ do
    handleDynamicFlags flags_
    action

handleStaticFlags :: [String] -> IO [Located String]
handleStaticFlags flags = do
  -- FIXME: use saveStaticFlagGlobals/restoreStaticFlagGlobals
  -- for ghc >= 7.2 instead of modifying v_opt_C_ready directly
  writeIORef v_opt_C_ready False
  fst `fmap` parseStaticFlags (map noLoc flags)

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
