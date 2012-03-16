{-# LANGUAGE ForeignFunctionInterface #-}
module ForeignImport where
import Foreign.C

import Prelude hiding (sin)
 
-- pure function
foreign import ccall "sin" c_sin :: CDouble -> CDouble
sin :: Double -> Double
sin d = realToFrac (c_sin (realToFrac d))
