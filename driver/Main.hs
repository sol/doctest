module Main (main) where
import           Test.DocTest
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= doctest
