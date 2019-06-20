module Foo where

import Data.Maybe

-- | Convert a map into list array.
-- prop> tabulate m !! fromEnum d == fromMaybe 0 (lookup d m)
tabulate :: [(Bool, Double)] -> [Double]
tabulate m = [fromMaybe 0 $ lookup False m, fromMaybe 0 $ lookup True m]
