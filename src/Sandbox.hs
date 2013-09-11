module Sandbox (getSandboxArguments) where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.System (buildPlatform)
import qualified Distribution.Text as Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (versionBranch, Version)
import System.Directory (getCurrentDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory)

configFile :: String
configFile = "cabal.sandbox.config"

sandboxDir :: String
sandboxDir = ".cabal-sandbox"

getSandboxArguments :: IO [String]
getSandboxArguments = do
    mdir <- getCurrentDirectory >>= getSandboxDir
    case mdir of
        Nothing   -> return []
        Just sdir -> sandboxArguments sdir

getSandboxDir :: FilePath -> IO (Maybe FilePath)
getSandboxDir dir = do
    exist <- doesSandboxExist dir
    if exist then
        return $ Just (dir </> sandboxDir)
      else do
        let dir' = takeDirectory dir
        if dir == dir' then
            return Nothing
          else
            getSandboxDir dir'

doesSandboxExist :: FilePath -> IO Bool
doesSandboxExist dir = do
    fileExist <- doesFileExist $ dir </> configFile
    dirExist <- doesDirectoryExist $ dir </> sandboxDir
    return (fileExist && dirExist)

sandboxArguments :: FilePath -> IO [String]
sandboxArguments sdir = do
    (strVer, ver) <- getGHCVersion
    let pkgDb = packageConfName strVer
        (pkgDbOpt,noUserPkgDbOpt)
          | ver >= 705 = ("-package-db",  "-no-user-package-db")
          | otherwise  = ("-package-conf","-no-user-package-conf")
        pkgDbPath = sdir </> pkgDb
        libPath   = sdir </> "lib"
        impOpt = "-i" ++ libPath
    return [noUserPkgDbOpt, pkgDbOpt, pkgDbPath, impOpt]

packageConfName :: String-> FilePath
packageConfName strver = Text.display buildPlatform
                      ++ "-ghc-"
                      ++ strver
                      ++ "-packages.conf.d"

getGHCVersion :: IO (String, Int)
getGHCVersion = toTupple <$> getGHC
  where
    toTupple v
      | length vs < 2 = (verstr, 0)
      | otherwise     = (verstr, ver)
      where
        vs = versionBranch v
        ver = (vs !! 0) * 100 + (vs !! 1)
        verstr = intercalate "." . map show $ vs

getGHC :: IO Version
getGHC = do
    Just v <- programFindVersion ghcProgram silent (programName ghcProgram)
    return v
