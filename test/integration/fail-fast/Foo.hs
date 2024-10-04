module Foo where
import Bar -- force testing Foo before Bar

-- | A passing example
--
-- >>> 23
-- 23
test1 :: a
test1 = undefined

-- | A failing example
--
-- >>> 23
-- 42
test2 :: a
test2 = undefined

-- | Another passing example
--
-- >>> 23
-- 23
test3 :: a
test3 = undefined
