module DryR.Util where

import Data.List
import Data.Bits
import Data.Word

fromOctets :: [Word8] -> Word32
fromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o
