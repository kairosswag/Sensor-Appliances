{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Init where

import DryR.Context
import DryR.DBus.MethodCall.Adapter1

initialize :: InnerContext -> IO (Maybe ())
initialize = withContext (\c -> do
  powerOn c "/org/bluez/hci0"
  startDiscovery c "/org/bluez/hci0"
  return ())

uninitialize :: InnerContext -> IO (Maybe ())
uninitialize = withContext (\c -> do
  stopDiscovery c "/org/bluez/hci0"
  powerOff c "/org/bluez/hci0"
  return ())
