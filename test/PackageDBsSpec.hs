module PackageDBsSpec (main, spec) where

import           Prelude ()
import           Prelude.Compat

import qualified Control.Exception         as E
import           Data.List                 (intercalate)
import           PackageDBs
import           System.Directory          (getCurrentDirectory, setCurrentDirectory)
import           System.Environment.Compat
import           System.FilePath           (searchPathSeparator)
import           Test.Hspec

import           Test.Mockery.Directory

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
clearEnv =
    withEnv "GHC_PACKAGE_PATH" ""
  . withEnv "HASKELL_PACKAGE_SANDBOX" ""
  . withEnv "HASKELL_PACKAGE_SANDBOXES" ""

combineDirs :: [FilePath] -> String
combineDirs = intercalate [searchPathSeparator]

spec :: Spec
spec = around_ clearEnv $ do
  describe "getPackageDBsFromEnv" $ do
    context "without a cabal sandbox present" $ do
      around_ (inTempDirectory) $ do
        it "uses global and user when no env or sandboxing used" $ do
          getPackageDBsFromEnv `shouldReturn` PackageDBs True True [] Nothing

        it "respects GHC_PACKAGE_PATH" $
          withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo", "bar", ""]) $ do
            getPackageDBsFromEnv `shouldReturn` PackageDBs False True ["foo", "bar"] Nothing

        it "HASKELL_PACKAGE_SANDBOXES trumps GHC_PACKAGE_PATH" $
          withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo1", "bar1", ""]) $ do
          withEnv "HASKELL_PACKAGE_SANDBOXES" (combineDirs ["foo2", "bar2", ""]) $ do
            getPackageDBsFromEnv `shouldReturn` PackageDBs False True ["foo2", "bar2"] Nothing

        it "HASKELL_PACKAGE_SANDBOX trumps GHC_PACKAGE_PATH" $
          withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo1", "bar1", ""]) $ do
          withEnv "HASKELL_PACKAGE_SANDBOX" (combineDirs ["foo2"]) $ do
            getPackageDBsFromEnv `shouldReturn` PackageDBs True True ["foo2"] Nothing

        it "HASKELL_PACKAGE_SANDBOX with explicit package ids" $
          withEnv "HASKELL_PACKAGE_SANDBOX" (combineDirs ["foo2"]) $ do
          withEnv "HASKELL_PACKAGE_IDS" (unwords ["pkg1-1.0", "pkgX-1.1.1-HASHHERE"]) $ do
            getPackageDBsFromEnv `shouldReturn` PackageDBs True True ["foo2"] (Just ["pkg1-1.0", "pkgX-1.1.1-HASHHERE"])

    context "with a cabal sandbox present" $ do
      around_ (withCurrentDirectory "test/sandbox") $ do
        it "respects cabal sandboxes" $ do
            getPackageDBsFromEnv `shouldReturn`
              PackageDBs False True ["/home/me/doctest-haskell/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"] Nothing

        it "GHC_PACKAGE_PATH takes precedence" $
          withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo", "bar"]) $ do
            getPackageDBsFromEnv `shouldReturn`
              PackageDBs False False ["foo", "bar"] Nothing
