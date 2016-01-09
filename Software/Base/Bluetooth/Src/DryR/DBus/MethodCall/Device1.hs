{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodCall.Device1 where

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.Properties

connect :: Context -> ObjectPath -> IO (Maybe ())
connect c oP = do
  let mc = (methodCall oP ("org.bluez.Device1") ("Connect")) {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()

disconnect :: Context -> ObjectPath -> IO (Maybe ())
disconnect c oP = do
  let mc = (methodCall oP ("org.bluez.Device1") ("Disconnect")) {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()
