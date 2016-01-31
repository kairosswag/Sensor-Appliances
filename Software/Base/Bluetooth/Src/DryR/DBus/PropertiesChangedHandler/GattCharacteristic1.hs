{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler.GattCharacteristic1 where

import Data.Maybe

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.MethodCall.Properties
import DryR.DBus.PropertiesChanged
import DryR.Sensirion

import DryR.DBus.PropertiesChangedHandler.GattCharacteristic1.Humidity

gattCharacteristic1Handler pC c = do
  let oP = pCObjectPath pC
  u <- get c (busName_ "org.bluez") oP (interfaceName_ "org.bluez.GattCharacteristic1") (memberName_ "UUID")
  let us :: String = fromJust $ fromVariant $ fromJust u

  if us == ch_hum
    then humidityHandler pC c
    else return ()
