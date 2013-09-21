module Sandbox (getSandboxArguments, getPackageDbDir) where

import Control.Applicative ((<$>))
import Control.Exception as E (catch, SomeException, throwIO)
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)
import System.Directory (getCurrentDirectory, doesFileExist)
import System.FilePath ((</>), takeDirectory, takeFileName)

configFile :: String
configFile = "cabal.sandbox.config"

pkgDbKey :: String
pkgDbKey = "package-db:"

pkgDbKeyLen :: Int
pkgDbKeyLen = length pkgDbKey

getSandboxArguments :: IO [String]
getSandboxArguments = (sandboxArguments <$> getPkgDb) `E.catch` handler
  where
    getPkgDb = getCurrentDirectory >>= getSandboxConfigFile >>= getPackageDbDir
    handler :: SomeException -> IO [String]
    handler _ = return []

-- | Find a sandbox config file by tracing ancestor directories.
--   Exception is thrown if not found
getSandboxConfigFile :: FilePath -> IO FilePath
getSandboxConfigFile dir = do
    let cfile = dir </> configFile
    exist <- doesFileExist cfile
    if exist then
        return cfile
      else do
        let dir' = takeDirectory dir
        if dir == dir' then
            throwIO $ userError "sandbox config file not found"
          else
            getSandboxConfigFile dir'

-- | Extract a package db directory from the sandbox config file.
--   Exception is thrown if the sandbox config file is broken.
getPackageDbDir :: FilePath -> IO FilePath
getPackageDbDir sconf = extractValue . parse <$> readFile sconf
  where
    parse = head . filter ("package-db:" `isPrefixOf`) . lines
    extractValue = fst . break isSpace . dropWhile isSpace . drop pkgDbKeyLen

-- | Adding necessary GHC options to the package db.
--   Exception is thrown if the string argument is incorrect.
--
-- >>> sandboxArguments "/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"
-- ["-no-user-package-db","-package-db","/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"]
-- >>> sandboxArguments "/foo/bar/i386-osx-ghc-7.4.1-packages.conf.d"
-- ["-no-user-package-conf","-package-conf","/foo/bar/i386-osx-ghc-7.4.1-packages.conf.d"]
sandboxArguments :: FilePath -> [String]
sandboxArguments pkgDb = [noUserPkgDbOpt, pkgDbOpt, pkgDb]
  where
    ver = extractGhcVer pkgDb
    (pkgDbOpt,noUserPkgDbOpt)
      | ver < 706 = ("-package-conf","-no-user-package-conf")
      | otherwise = ("-package-db",  "-no-user-package-db")

-- | Extracting GHC version from the path of package db.
--   Exception is thrown if the string argument is incorrect.
--
-- >>> extractGhcVer "/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"
-- 706
extractGhcVer :: String -> Int
extractGhcVer dir = ver
  where
    file = takeFileName dir
    findVer = drop 4 . head . filter ("ghc-" `isPrefixOf`) . tails
    (verStr1,_:left) = break (== '.') $ findVer file
    (verStr2,_)      = break (== '.') left
    ver = read verStr1 * 100 + read verStr2
