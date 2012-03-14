import qualified Interpreter

main = Interpreter.withInterpreter [] $ \repl -> do
  putStrLn "sleeping for 1 second..."
  Interpreter.eval repl "import Control.Concurrent"
  Interpreter.eval repl "threadDelay 1000000"
  putStrLn "done"
