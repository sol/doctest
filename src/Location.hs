{-# LANGUAGE CPP #-}
module Location where

import           Control.DeepSeq (deepseq, NFData(rnf))
import           SrcLoc
import           FastString (unpackFS)
import           Outputable (showPpr)

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
noLocation a = (UnhelpfulLocation "<no location info>", a)

toLocation :: SrcSpan -> Location
#if __GLASGOW_HASKELL__ < 702
toLocation loc
  | isGoodSrcLoc start = Location (unpackFS $ srcLocFile start) (srcLocLine start)
  | otherwise          = (UnhelpfulLocation . showPpr) start
  where
    start = srcSpanStart loc
#else
toLocation loc = case loc of
  UnhelpfulSpan str -> UnhelpfulLocation (unpackFS str)
  RealSrcSpan sp    -> Location (unpackFS . srcSpanFile $ sp) (srcSpanStartLine sp)
#endif
