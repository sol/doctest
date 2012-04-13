module Count (
    Count(..)
  , countModules
  , countModules'
  ) where

import Data.Monoid

import Parse

countModules :: [Module Example] -> (Int, Int)
countModules m = (exampleCount c, interactionCount c)
  where
    c = countModules' m

countModules' :: [Module Example] -> Count
countModules' = mconcat . map count

-- | Number of examples and interactions.
data Count = Count {
    exampleCount :: Int
  , interactionCount :: Int
  }

instance Monoid Count where
  mempty = Count 0 0
  (Count x1 y1) `mappend` (Count x2 y2) = Count (x1 + x2) (y1 + y2)

instance Show Count where
  show (Count 1 1)           = "There is one test, with one single interaction."
  show (Count 1 iCount)      = "There is one test, with " ++ show iCount ++ " interactions."
  show (Count tCount iCount) = "There are " ++ show tCount ++ " tests, with " ++ show iCount ++ " total interactions."

-- | Count number of examples and interactions in given module.
count :: Module Example -> Count
count (Module _ examples) = (mconcat . map f) examples
  where
    f :: Example -> Count
    f (Example x) = Count 1 (length x)
