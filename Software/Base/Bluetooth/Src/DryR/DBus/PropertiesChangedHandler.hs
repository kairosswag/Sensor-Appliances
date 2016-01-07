{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler (propertiesChangedHandler) where

import DBus
import DBus.Client

import DryR.DBus.PropertiesChanged

import DryR.DBus.PropertiesChangedHandler.Adapter1
import DryR.DBus.PropertiesChangedHandler.Device1
import DryR.DBus.PropertiesChangedHandler.GattCharacteristic1
import DryR.DBus.PropertiesChangedHandler.GattDescriptor1
import DryR.DBus.PropertiesChangedHandler.GattService1

propertiesChangedHandler :: PropertiesChanged -> Client -> IO ()
propertiesChangedHandler pC = case (formatInterfaceName $ pCInterfaceName pC) of
  "org.bluez.Adapter1" -> adapter1Handler pC
  "org.bluez.Device1" -> device1Handler pC
  "org.bluez.GattService1" -> gattService1Handler pC
  "org.bluez.GattCharacteristic1" -> gattCharacteristic1Handler pC
  "org.bluez.GattDescriptor1" -> gattDescriptor1Handler pC
  _ -> const $ return ()
