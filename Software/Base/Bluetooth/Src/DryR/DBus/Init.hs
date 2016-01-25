{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.Init where

import qualified Data.Map.Lazy as LM

import Database.MySQL.Simple

import DBus

import DryR.Context
import DryR.DBus.MethodCall.Adapter1
import DryR.DBus.MethodCall.Device1
import DryR.DBus.Util
import DryR.SQL.Query
import DryR.Sensirion

initialize :: InnerContext -> IO (Maybe ())
initialize = withContext (\c -> do
  --let uuidsFilter = ("UUIDs" :: String, toVariant $ [sv_hum, sv_tmp])
  let transportFilter = ("Transport" :: String, toVariant $ ("le" :: String))
  let filters = LM.fromList [transportFilter]

  powerOn c "/org/bluez/hci0"
  setDiscoveryFilter c "/org/bluez/hci0" filters
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
