{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import           Parse
import           Location

-- The generic form
--
-- > deriving instance Show a => Show (Module a)
--
-- fails with GHC 7.0.1 due to an overlapping instance (leaked by the GHC API),
-- this is why we derive the things we need individually.
deriving instance Show (Module String)
deriving instance Show (Module [DocTest])
deriving instance Show (Module [Located DocTest])
