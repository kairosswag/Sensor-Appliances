{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodCall.Adapter1 where

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.Properties

powerOn :: Context -> ObjectPath -> IO (Maybe ())
powerOn c oP = set c "org.bluez" oP "org.bluez.Adapter1" "Powered" True

powerOff :: Context -> ObjectPath -> IO (Maybe ())
powerOff c oP = set c "org.bluez" oP "org.bluez.Adapter1" "Powered" False

startDiscovery :: Context -> ObjectPath -> IO (Maybe ())
startDiscovery c oP = do
  let mc = (methodCall oP ("org.bluez.Adapter1") ("StartDiscovery")) {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()

stopDiscovery :: Context -> ObjectPath -> IO (Maybe ())
stopDiscovery c oP = do
  let mc = (methodCall oP ("org.bluez.Adapter1") ("StopDiscovery")) {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()

removeDevice :: Context -> ObjectPath -> ObjectPath -> IO (Maybe ())
removeDevice c oP device = do
  let mc = (methodCall oP ("org.bluez.Adapter1") ("RemoveDevice")) {
    methodCallDestination = Just "org.bluez",
    methodCallBody = [toVariant device]
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left e -> return Nothing
    Right s -> return $ Just ()
