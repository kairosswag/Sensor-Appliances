{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler (propertiesChangedHandler) where

import Data.Maybe
import Data.Word
import qualified Data.ByteString as BS

import DBus
import DBus.Client
import DBus.Introspection

import Data.Binary.IEEE754

import DryR.DBus.Properties
import DryR.DBus.PropertiesChanged
import DryR.Sensirion
import DryR.Util

propertiesChangedHandler :: PropertiesChanged -> Client -> IO ()
propertiesChangedHandler pC = case (formatInterfaceName $ pCInterfaceName pC) of
  "org.bluez.Adapter1" -> a1Handler pC
  "org.bluez.Device1" -> d1Handler pC
  "org.bluez.GattService1" -> gS1Handler pC
  "org.bluez.GattCharacteristic1" -> gC1Handler pC
  "org.bluez.GattDescriptor1" -> gD1Handler pC
  _ -> const $ return ()

a1Handler pC client = print "A1"
d1Handler pC client = print "D1"
gS1Handler pC client = print "GS1"
gC1Handler pC client = do
  let oP = pCObjectPath pC
  print $ formatObjectPath oP
  u <- get client (busName_ "org.bluez") oP (interfaceName_ "org.bluez.GattCharacteristic1") (memberName_ "UUID")
  let us :: String = fromJust $ fromVariant $ fromJust $ fromVariant $ fromJust u

  if us == ch_tmp
    then gC1HHandler pC client
    else if us == ch_hum
      then gC1THandler pC client
      else return ()

  print "GC1"

gC1THandler pC client = do
  let oP = pCObjectPath pC
  let cP = pCChangedProperties pC

  let i = head $ filter (\(k,_) -> (fromJust $ fromVariant k) == "Value") (dictionaryItems cP)
  let v :: BS.ByteString = fromJust $ fromVariant $ fromJust $ fromVariant $ snd i

  let f = wordToFloat $ fromOctets $ reverse $ BS.unpack v

  print $ f

gC1HHandler pC client = do
  let oP = pCObjectPath pC
  let cP = pCChangedProperties pC

  print cP

gD1Handler pC client = print "GD1"
