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
import Control.Exception (try, SomeException)
import System.Directory (getCurrentDirectory)

-- | Full stack of GHC package databases
data PackageDBs = PackageDBs
    { includeUser :: Bool
    -- | Unsupported on GHC < 7.6
    , includeGlobal :: Bool
    , extraDBs :: [FilePath]
    }
    deriving (Show, Eq)

-- | Package database handling switched between GHC 7.4 and 7.6
data ArgStyle = Pre76 | Post76
    deriving (Show, Eq)

-- | Determine command line arguments to be passed to GHC to set databases correctly
--
-- >>> dbArgs Post76 (PackageDBs False True [])
-- ["-no-user-package-db"]
--
-- >>> dbArgs Pre76 (PackageDBs True True ["somedb"])
-- ["-package-conf","somedb"]
dbArgs :: ArgStyle -> PackageDBs -> [String]
dbArgs Post76 (PackageDBs user global extras) =
    (if user then id else ("-no-user-package-db":)) $
    (if global then id else ("-no-global-package-db":)) $
    concatMap (\extra -> ["-package-db", extra]) extras
dbArgs Pre76 (PackageDBs _ False _) =
    error "Global package database must be included with GHC < 7.6"
dbArgs Pre76 (PackageDBs user True extras) =
    (if user then id else ("-no-user-package-conf":)) $
    concatMap (\extra -> ["-package-conf", extra]) extras

-- | The argument style to be used with the current GHC version
buildArgStyle :: ArgStyle
#if __GLASGOW_HASKELL__ >= 706
buildArgStyle = Post76
#else
buildArgStyle = Pre76
#endif

-- | Determine the PackageDBs based on the environment.
getPackageDBsFromEnv :: IO PackageDBs
getPackageDBsFromEnv = do
    env <- getEnvironment
    return $ case () of
        ()
            | Just packageDBs <- lookup "GHC_PACKAGE_PATH" env
                -> fromEnvMulti packageDBs
            | otherwise
                -> PackageDBs True True []
  where
    fromEnvMulti s = PackageDBs
        { includeUser = False
        , includeGlobal = global
        , extraDBs = splitSearchPath s'
        }
      where
        (s', global) =
            case reverse s of
                c:rest | c == searchPathSeparator -> (reverse rest, True)
                _ -> (s, False)

-- | Get the package DB flags for the current GHC version and from the
-- environment.
getPackageDBArgs :: IO [String]
getPackageDBArgs = do
      dbs <- getPackageDBsFromEnv
      return $ dbArgs buildArgStyle dbs
