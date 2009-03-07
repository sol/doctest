module Test.DocTest.Util where

import Data.List


-- Source: Posted from Chaddaï Fouché to Haskell-Cafe mailing list.
--
-- Example:
-- > replace "." "/" "Foo.Bar.Baz"
-- "Foo/Bar/Baz"
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
	case stripPrefix old xs of
		Nothing -> y : replace old new ys
		Just ys' -> new ++ replace old new ys' 
