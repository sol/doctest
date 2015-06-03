module PackageDBsSpec (main, spec) where

import qualified Control.Exception         as E
import           Data.List                 (intercalate)
import           PackageDBs
import           System.Directory          (getCurrentDirectory,
                                            removeDirectoryRecursive,
                                            setCurrentDirectory)
import           System.Environment.Compat
import           System.FilePath           (searchPathSeparator)
import           Test.Hspec

main :: IO ()
main = hspec spec

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  E.bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = E.bracket save restore $ \_ -> do
  setEnv k v >> action
  where
    save    = lookup k <$> getEnvironment
    restore = maybe (unsetEnv k) (setEnv k)

clearEnv :: IO a -> IO a
clearEnv = withEnv "GHC_PACKAGE_PATH" ""
         . withEnv "HASKELL_PACKAGE_SANDBOX" ""
         . withEnv "HASKELL_PACKAGE_SANDBOXES" ""

combineDirs :: [FilePath] -> String
combineDirs = intercalate [searchPathSeparator]

spec :: Spec
spec = do
  describe "getPackageDBsFromEnv" $ do
    it "uses global and user when no env or sandboxing used" $
      withCurrentDirectory "test" $ clearEnv $ do
        dbs <- getPackageDBsFromEnv
        dbs `shouldBe` PackageDBs True True []
    it "respects GHC_PACKAGE_PATH" $
      withCurrentDirectory "test" $ clearEnv $
      withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo", "bar", ""]) $ do
        dbs <- getPackageDBsFromEnv
        dbs `shouldBe` PackageDBs False True ["foo", "bar"]
    it "HASKELL_PACKAGE_SANDBOXES trumps GHC_PACKAGE_PATH" $
      withCurrentDirectory "test" $ clearEnv $
      withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo1", "bar1", ""]) $
      withEnv "HASKELL_PACKAGE_SANDBOXES" (combineDirs ["foo2", "bar2", ""]) $ do
        dbs <- getPackageDBsFromEnv
        dbs `shouldBe` PackageDBs False True ["foo2", "bar2"]
    it "HASKELL_PACKAGE_SANDBOX trumps GHC_PACKAGE_PATH" $
      withCurrentDirectory "test" $ clearEnv $
      withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo1", "bar1", ""]) $
      withEnv "HASKELL_PACKAGE_SANDBOX" (combineDirs ["foo2"]) $ do
        dbs <- getPackageDBsFromEnv
        dbs `shouldBe` PackageDBs True True ["foo2"]
    it "respects cabal sandboxes" $
      withCurrentDirectory "test/sandbox" $ clearEnv $ do
        dbs <- getPackageDBsFromEnv
        dbs `shouldBe` PackageDBs False True ["/home/me/doctest-haskell/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
    it "env trumps cabal sandboxes" $
      withCurrentDirectory "test/sandbox" $ clearEnv $
      withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo", "bar"]) $ do
        dbs <- getPackageDBsFromEnv
        dbs `shouldBe` PackageDBs False False ["foo", "bar"]
