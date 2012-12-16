{-# LANGUAGE ScopedTypeVariables #-}
module Paths (ghc, libdir) where

import           Control.Exception
import qualified GHC.Paths
import           System.Environment
import           System.IO.Unsafe

envorghc :: String -> String -> String
envorghc envv ghcv = unsafePerformIO $ do l <- try $ getEnv envv
                                          case l of
                                            Left (_ :: SomeException) -> return ghcv
                                            Right v -> return v

ghc :: String
ghc = envorghc "DOCTESTS_GHC" GHC.Paths.ghc

libdir :: String
libdir = envorghc "DOCTESTS_GHC_LIBDIR" GHC.Paths.libdir
