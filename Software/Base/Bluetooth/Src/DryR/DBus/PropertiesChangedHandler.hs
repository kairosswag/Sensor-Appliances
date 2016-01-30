{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler (propertiesChangedHandler) where

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.PropertiesChanged

import DryR.DBus.PropertiesChangedHandler.Adapter1
import DryR.DBus.PropertiesChangedHandler.Device1
import DryR.DBus.PropertiesChangedHandler.GattCharacteristic1
import DryR.DBus.PropertiesChangedHandler.GattDescriptor1
import DryR.DBus.PropertiesChangedHandler.GattService1

propertiesChangedHandler :: PropertiesChanged -> Context -> IO ()
propertiesChangedHandler pC c = do
  print pC
  case (formatInterfaceName $ pCInterfaceName pC) of
    "org.bluez.Adapter1" -> adapter1Handler pC c
    "org.bluez.Device1" -> device1Handler pC c
    "org.bluez.GattService1" -> gattService1Handler pC c
    "org.bluez.GattCharacteristic1" -> gattCharacteristic1Handler pC c
    "org.bluez.GattDescriptor1" -> gattDescriptor1Handler pC c
    _ -> return ()
