{-# LANGUAGE CPP, DeriveFunctor #-}
module Location where

import           Control.DeepSeq (deepseq, NFData(rnf))
import           SrcLoc hiding (Located)
import qualified SrcLoc as GHC
import           FastString (unpackFS)

#if __GLASGOW_HASKELL__ < 702
import           Outputable (showPpr)
#endif

-- | A thing with a location attached.
data Located a = Located Location a
  deriving (Eq, Show, Functor)

instance NFData a => NFData (Located a) where
  rnf (Located loc a) = loc `deepseq` a `deepseq` ()

-- | Convert a GHC located thing to a located thing.
toLocated :: GHC.Located a -> Located a
toLocated (L loc a) = Located (toLocation loc) a

-- | Discard location information.
unLoc :: Located a -> a
unLoc (Located _ a) = a

-- | Add dummy location information.
noLocation :: a -> Located a
noLocation = Located (UnhelpfulLocation "<no location info>")

-- | A line number.
type Line = Int

-- | A combination of file name and line number.
data Location = UnhelpfulLocation String | Location FilePath Line
  deriving (Eq, Show)

instance NFData Location where
  rnf (UnhelpfulLocation str) = str `deepseq` ()
  rnf (Location file line)    = file `deepseq` line `deepseq` ()

-- |
-- Create a list from a location, by repeatedly increasing the line number by
-- one.
enumerate :: Location -> [Location]
enumerate loc = case loc of
  UnhelpfulLocation _ -> repeat loc
  Location file line  -> map (Location file) [line ..]

-- | Convert a GHC source span to a location.
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
