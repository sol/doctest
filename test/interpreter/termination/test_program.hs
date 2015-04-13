import           Control.Exception
import           Language.Haskell.GhciWrapper

main :: IO ()
main = bracket (new []) close $ \repl -> do
  putStrLn "sleeping for 1 second..."
  _ <- eval repl "import Control.Concurrent"
  _ <- eval repl "threadDelay 1000000"
  putStrLn "done"
