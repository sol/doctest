module TestA (foo) where

import TestB ()

{- $setup
>>> :set -XTypeApplications
-}

-- | Example usage:
--
-- >>> foo @Int
-- 3
foo :: Num a => a
foo = 3
