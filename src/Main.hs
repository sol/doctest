module Main (main) where
import           Run
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= doctest
