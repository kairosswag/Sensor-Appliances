module DryR.Util where

import Data.List
import Data.Maybe
import Data.Bits
import Data.Word

import qualified Data.ByteString as BS

import Data.Binary.IEEE754

word8ListToWord32 :: [Word8] -> Maybe Word32
word8ListToWord32 w8l = if length w8l == 4
  then Just $ foldl' accum 0 w8l
  else Nothing
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

word8ListToFloat :: [Word8] -> Maybe Float
word8ListToFloat w8l = fmap wordToFloat $ word8ListToWord32 w8l
