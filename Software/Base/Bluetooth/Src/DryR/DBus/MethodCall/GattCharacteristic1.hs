{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodCall.GattCharacteristic1 where

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.Properties

startNotify :: Context -> ObjectPath -> IO (Maybe ())
startNotify c oP = do
  let mc = (methodCall oP ("org.bluez.GattCharacteristic1") ("StartNotify")) {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()

stopNotify :: Context -> ObjectPath -> IO (Maybe ())
stopNotify c oP = do
  let mc = (methodCall oP ("org.bluez.GattCharacteristic1") ("StopNotify")) {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()
