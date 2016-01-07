{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler.GattCharacteristic1 where

import Data.Maybe

import DBus
import DBus.Client

import DryR.DBus.Properties
import DryR.DBus.PropertiesChanged
import DryR.Sensirion

import DryR.DBus.PropertiesChangedHandler.GattCharacteristic1.Humidity
import DryR.DBus.PropertiesChangedHandler.GattCharacteristic1.Temperature

gattCharacteristic1Handler pC client = do
  let oP = pCObjectPath pC
  print $ formatObjectPath oP
  u <- get client (busName_ "org.bluez") oP (interfaceName_ "org.bluez.GattCharacteristic1") (memberName_ "UUID")
  let us :: String = fromJust $ fromVariant $ fromJust u

  if us == ch_tmp
    then temperatureHandler pC client
    else if us == ch_hum
      then humidityHandler pC client
      else return ()

  print "GC1"
