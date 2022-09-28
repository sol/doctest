{-# LANGUAGE CPP #-}
-- | Manage GHC package databases
module PackageDBs (
  getPackageDBArgs
#ifdef TEST
, PackageDBs (..)
, getPackageDBsFromEnv
#endif
) where

import System.Environment (getEnvironment)
import System.FilePath (splitSearchPath, searchPathSeparator)

-- | Full stack of GHC package databases
data PackageDBs = PackageDBs
    { includeUser :: Bool
    , includeGlobal :: Bool
    , extraDBs :: [FilePath]
    }
    deriving (Show, Eq)

-- | Determine command line arguments to be passed to GHC to set databases correctly
--
-- >>> dbArgs (PackageDBs False True [])
-- ["-no-user-package-db"]
--
-- >>> dbArgs (PackageDBs True True ["somedb"])
-- ["-package-db","somedb"]
dbArgs :: PackageDBs -> [String]
dbArgs (PackageDBs user global extras) =
    (if user then id else ("-no-user-package-db":)) $
    (if global then id else ("-no-global-package-db":)) $
    concatMap (\extra -> ["-package-db", extra]) extras

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
      return $ dbArgs dbs
