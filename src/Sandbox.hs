module Sandbox (getSandboxArguments) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)
import System.Directory (getCurrentDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory, takeFileName)
import Control.Exception (handle, SomeException)

configFile :: String
configFile = "cabal.sandbox.config"

sandboxDir :: String
sandboxDir = ".cabal-sandbox"

pkgDbKey :: String
pkgDbKey = "package-db:"

pkgDbKeyLen :: Int
pkgDbKeyLen = length pkgDbKey

getSandboxArguments :: IO [String]
getSandboxArguments = do
    mdir <- getCurrentDirectory >>= getSandboxDir
    case mdir of
        Nothing           -> return []
        Just (sdir,sconf) -> sandboxArguments sdir sconf

getSandboxDir :: FilePath -> IO (Maybe (FilePath,FilePath))
getSandboxDir dir = do
    exist <- doesSandboxExist dir
    if exist then
        return $ Just (dir </> sandboxDir, dir </> configFile)
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

sandboxArguments :: FilePath -> FilePath -> IO [String]
sandboxArguments sdir sconf = handle handler $ do
    pkgDb <- getPackageDbDir sconf
    let ver = extractGhcVer pkgDb
    let (pkgDbOpt,noUserPkgDbOpt)
          | ver < 706 = ("-package-conf","-no-user-package-conf")
          | otherwise = ("-package-db",  "-no-user-package-db")
        pkgDbPath = sdir </> pkgDb
        libPath   = sdir </> "lib"
        impOpt = "-i" ++ libPath
    return [noUserPkgDbOpt, pkgDbOpt, pkgDbPath, impOpt]
  where
    handler :: SomeException -> IO [String]
    handler _ = return []

getPackageDbDir :: FilePath -> IO FilePath
getPackageDbDir sconf = do
    ls <- lines <$> readFile sconf
    let [target] = filter ("package-db:" `isPrefixOf`) ls
    return $ extractValue target
  where
    extractValue = fst . break isSpace . dropWhile isSpace . drop pkgDbKeyLen

extractGhcVer :: String -> Int
extractGhcVer dir = ver
  where
    file = takeFileName dir
    findVer = drop 4 . head . filter ("ghc-" `isPrefixOf`) . tails
    (verStr1,_:left) = break (== '.') $ findVer file
    (verStr2,_)      = break (== '.') left
    ver = read verStr1 * 100 + read verStr2
