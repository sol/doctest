module Location where

import           Control.DeepSeq (deepseq, NFData(rnf))

type Line = Int

data Location = UnhelpfulLocation String | Location FilePath Line
  deriving (Eq, Show)

instance NFData Location where
  rnf (UnhelpfulLocation str) = str `deepseq` ()
  rnf (Location file line)    = file `deepseq` line `deepseq` ()

enumerate :: Location -> [Location]
enumerate loc = case loc of
  UnhelpfulLocation _ -> repeat loc
  Location file line  -> map (Location file) [line ..]

noLocation :: a -> (Location, a)
noLocation a = (UnhelpfulLocation "no location", a)
