module ThreadPool (makeThreadPool) where

import Prelude hiding (map)

import Control.Concurrent (Chan, newChan, readChan, forkIO)
import Control.Monad (forM_, forever)

makeThreadPool :: Int -> (Chan b -> a -> IO ()) -> IO (Chan a, Chan b)
makeThreadPool nThreads mutator = do
  input <- newChan
  output <- newChan
  forM_ [1..nThreads] $ \_ ->
    forkIO $ forever $ do
      i <- readChan input
      mutator output i
  return (input, output)
