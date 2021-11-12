module Info where

import           Data.List

formatInfo :: [(String, String)] -> String
formatInfo xs = " [" ++ (intercalate "\n ," $ map show xs) ++ "\n ]\n"
