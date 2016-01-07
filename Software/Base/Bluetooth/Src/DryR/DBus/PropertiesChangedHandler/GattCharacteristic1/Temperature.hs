{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler.GattCharacteristic1.Temperature where

import Data.Maybe
import Data.Word
import qualified Data.ByteString as BS

import Data.Binary.IEEE754

import DBus
import DBus.Client

import DryR.DBus.PropertiesChanged
import DryR.Util

temperatureHandler pC c = do
  let oP = pCObjectPath pC
  let cP = pCChangedProperties pC

  let i = head $ filter (\(k,_) -> (fromJust $ fromVariant k) == "Value") (dictionaryItems cP)
  let v :: BS.ByteString = fromJust $ fromVariant $ fromJust $ fromVariant $ snd i

  let f = wordToFloat $ fromOctets $ reverse $ BS.unpack v

  print $ f
