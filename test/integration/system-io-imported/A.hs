module A where

import System.IO

-- ghci-wrapper needs to poke around with System.IO itself, and unloads the module once it's done. Test to make sure legitimate uses of System.IO don't get lost in the wash.

-- |
-- >>> ReadMode
-- ReadMode

