module RewriteRulesWithSigs (foo) where

{-# RULES "map/append" forall f (xs :: [Int])  ys. map f (xs ++ ys) = map f xs ++ map f ys #-}

-- | doc for foo
foo :: Int
foo = 23
