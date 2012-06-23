{-# LANGUAGE TemplateHaskell #-}
--
-- derived from: http://www.haskell.org/ghc/docs/latest/html/users_guide/template-haskell.html#th-example
--
module Printf (pr) where

import Language.Haskell.TH

data Format = D | S | L String

parse :: String -> [Format]
parse s   = [ L s ]

gen :: [Format] -> Q Exp
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = stringE s

-- |
--
-- >>> :set -XTemplateHaskell
-- >>> putStrLn ( $(pr "Hello") )
-- Hello
pr :: String -> Q Exp
pr s = gen (parse s)
