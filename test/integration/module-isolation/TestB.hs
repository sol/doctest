module TestB (bar) where

-- | Example usage:
--
-- >>> bar @Int
-- 3
bar :: Num a => a
bar = 3
