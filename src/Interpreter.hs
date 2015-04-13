module Interpreter (
  Interpreter
, safeEval
, withInterpreter
, ghc
, interpreterSupported

-- exported for testing
, ghcInfo
, haveInterpreterKey
) where

import           System.Process
import           System.Directory (getPermissions, executable)
import           Control.Applicative
import           Control.Monad
import           Control.Exception hiding (handle)

import           Language.Haskell.GhciWrapper

haveInterpreterKey :: String
haveInterpreterKey = "Have interpreter"

ghcInfo :: IO [(String, String)]
ghcInfo = read <$> readProcess ghc ["--info"] []

interpreterSupported :: IO Bool
interpreterSupported = do
  -- in a perfect world this permission check should never fail, but I know of
  -- at least one case where it did..
  x <- getPermissions ghc
  unless (executable x) $ do
    fail $ ghc ++ " is not executable!"

  maybe False (== "YES") . lookup haveInterpreterKey <$> ghcInfo

-- | Run an interpreter session.
--
-- Example:
--
-- >>> withInterpreter [] $ \i -> eval i "23 + 42"
-- "65\n"
withInterpreter
  :: [String]               -- ^ List of flags, passed to GHC
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter flags = bracket (new flags) close
