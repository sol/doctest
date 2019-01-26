{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
-- | Manage GHC package databases
module PackageDBs
    ( PackageDBs (..)
    , ArgStyle (..)
    , dbArgs
    , buildArgStyle
    , getPackageDBsFromEnv
    , getPackageDBArgs
    ) where

import System.Environment (getEnvironment)
import System.FilePath (splitSearchPath, searchPathSeparator)
import qualified Sandbox
import Control.Exception (try, SomeException)
import System.Directory (getCurrentDirectory)

-- | Full stack of GHC package databases
data PackageDBs = PackageDBs
    { includeUser :: Bool
    -- | Unsupported on GHC < 7.6
    , includeGlobal :: Bool
    , extraDBs :: [FilePath]
    , explicitPackageIds :: Maybe [String]
    }
    deriving (Show, Eq)

-- | Package database handling switched between GHC 7.4 and 7.6
data ArgStyle = Pre76 | Post76
    deriving (Show, Eq)

-- | Determine command line arguments to be passed to GHC to set databases correctly
--
-- >>> dbArgs Post76 (PackageDBs False True [] (Just ["x-1.0.0-ABCDEFGH"]))
-- ["-no-user-package-db","-hide-all-packages","-package-id","x-1.0.0-ABCDEFGH"]
--
-- >>> dbArgs Pre76 (PackageDBs True True ["somedb"] Nothing)
-- ["-package-conf","somedb"]
dbArgs :: ArgStyle -> PackageDBs -> [String]
dbArgs Post76 (PackageDBs user global extras mPackageIds) =
    (if user then id else ("-no-user-package-db":)) $
    (if global then id else ("-no-global-package-db":)) $
    foldr (\extra -> ("-package-db":).( extra:)) (maybePackageIdArgs mPackageIds) extras
dbArgs Pre76 (PackageDBs _ False _ _) =
    error "Global package database must be included with GHC < 7.6"
dbArgs Pre76 (PackageDBs user True extras _) =
    (if user then id else ("-no-user-package-conf":)) $
    concatMap (\extra -> ["-package-conf", extra]) extras

-- | hide all packages and add explicit package ids if those
-- were specified
maybePackageIdArgs :: Maybe [String] -> [String]
maybePackageIdArgs Nothing = []
maybePackageIdArgs (Just pids) =
  "-hide-all-packages":
  concatMap (\extra -> ["-package-id", extra]) pids

-- | The argument style to be used with the current GHC version
buildArgStyle :: ArgStyle
#if __GLASGOW_HASKELL__ >= 706
buildArgStyle = Post76
#else
buildArgStyle = Pre76
#endif

-- | Determine the PackageDBs based on the environment and cabal sandbox
-- information
getPackageDBsFromEnv :: IO PackageDBs
getPackageDBsFromEnv = do
    env <- getEnvironment
    let packageIds = fmap words $ lookup "HASKELL_PACKAGE_IDS" env
        fromEnvMulti s = PackageDBs
            { includeUser = False
            , includeGlobal = global
            , extraDBs = splitSearchPath s'
            , explicitPackageIds = packageIds
            }
          where
            (s', global) =
                case reverse s of
                    c:rest | c == searchPathSeparator -> (reverse rest, True)
                    _ -> (s, False)
    case () of
        ()
            | Just sandboxes <- lookup "HASKELL_PACKAGE_SANDBOXES" env
                -> return $ fromEnvMulti sandboxes
            | Just extra <- lookup "HASKELL_PACKAGE_SANDBOX" env
                -> return PackageDBs
                    { includeUser = True
                    , includeGlobal = True
                    , extraDBs = [extra]
                    , explicitPackageIds = packageIds
                    }
            | Just sandboxes <- lookup "GHC_PACKAGE_PATH" env
                -> return $ fromEnvMulti sandboxes
            | otherwise -> do
                eres <- try $ getCurrentDirectory
                          >>= Sandbox.getSandboxConfigFile
                          >>= Sandbox.getPackageDbDir
                return $ case eres :: Either SomeException FilePath of
                    Left _ -> PackageDBs True True [] Nothing
                    Right db -> PackageDBs False True [db] Nothing

-- | Get the package DB flags for the current GHC version and from the
-- environment.
getPackageDBArgs :: IO [String]
getPackageDBArgs = do
      dbs <- getPackageDBsFromEnv
      return $ dbArgs buildArgStyle dbs
