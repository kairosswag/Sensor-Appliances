{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.Init where

import Database.MySQL.Simple

import DryR.Context
import DryR.DBus.MethodCall.Adapter1
import DryR.DBus.MethodCall.Device1
import DryR.DBus.Util
import DryR.SQL.Query

initialize :: InnerContext -> IO (Maybe ())
initialize = withContext (\c -> do
  powerOn c "/org/bluez/hci0"
  startDiscovery c "/org/bluez/hci0"
  return ())

uninitialize :: InnerContext -> IO (Maybe ())
uninitialize = withContext (\c -> do

  qr :: [Only String] <- query_ (contextDatabase c) (getQuery SelectDeviceConnected $ contextQueries c)
  let connected_devices = map ((macToObjectPath "/org/bluez/hci0") . fromOnly) qr

  mapM_ (disconnect c) connected_devices

  stopDiscovery c "/org/bluez/hci0"
  powerOff c "/org/bluez/hci0"
  return ())
