module PackageDBsSpec (main, spec) where

import qualified Control.Exception         as E
import           Data.List                 (intercalate)
import           PackageDBs
import           System.Environment
import           System.FilePath           (searchPathSeparator)
import           Test.Hspec

import           Test.Mockery.Directory

main :: IO ()
main = hspec spec

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = E.bracket save restore $ \_ -> do
  setEnv k v >> action
  where
    save    = lookup k <$> getEnvironment
    restore = maybe (unsetEnv k) (setEnv k)

clearEnv :: IO a -> IO a
clearEnv =
    withEnv "GHC_PACKAGE_PATH" ""

combineDirs :: [FilePath] -> String
combineDirs = intercalate [searchPathSeparator]

spec :: Spec
spec = around_ clearEnv $ do
  describe "getPackageDBsFromEnv" $ do
    around_ (inTempDirectory) $ do
      it "uses global and user when no env used" $ do
        getPackageDBsFromEnv `shouldReturn` PackageDBs True True []

      it "respects GHC_PACKAGE_PATH" $
        withEnv "GHC_PACKAGE_PATH" (combineDirs ["foo", "bar", ""]) $ do
          getPackageDBsFromEnv `shouldReturn` PackageDBs False True ["foo", "bar"]
