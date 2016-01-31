{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DryR.DBus.Init where

import Data.Maybe
import Data.List
import Data.Int
import qualified Data.Map as M

import Database.MySQL.Simple

import DBus

import DryR.Context
import DryR.DBus.MethodCall.Adapter1
import DryR.DBus.MethodCall.Device1
import DryR.DBus.MethodCall.ObjectManager
import DryR.DBus.Util
import DryR.SQL.Query
import DryR.Sensirion

initialize :: InnerContext -> IO (Maybe ())
initialize = withContext (\c -> do
  --let uuidsFilter = ("UUIDs" :: String, toVariant $ [sv_hum, sv_tmp])
  let transportFilter = ("Transport" :: String, toVariant $ ("le" :: String))
  let filters = M.fromList [transportFilter]

  mmO <- getManagedObjects c
  let mDv = filter (\(_, i) -> M.member "org.bluez.Device1" i) $
            M.assocs $ fromJust mmO
  let macs = map (fromJust.objectPathToMac.fst) mDv
  let mDSF = map (, 0 :: Integer, minBound :: Int16) macs

  qr :: [Only String] <- query_ (contextDatabase c) (getQuery SelectDevice $ contextQueries c)
  let mDSN = map (,2 :: Integer, minBound :: Int16) ((map (fromOnly) qr)\\macs)

  let mDS =  union mDSF mDSN
  let mDSP = map (\(m,s,r) -> (m,s,r,s,r)) mDS

  print mDSP

  mapM (execute (contextDatabase c) (getQuery InsertDeviceOrUpdate $ contextQueries c)) mDSP

  powerOn c (contextAdapter c)
  setDiscoveryFilter c (contextAdapter c) filters
  startDiscovery c (contextAdapter c)
  return ())

uninitialize :: InnerContext -> IO (Maybe ())
uninitialize = withContext (\c -> do

  qr :: [Only String] <- query_ (contextDatabase c) (getQuery SelectDeviceConnected $ contextQueries c)
  let connected_devices = map ((macToObjectPath (contextAdapter c)) . fromOnly) qr

  mapM_ (disconnect c) connected_devices

  stopDiscovery c (contextAdapter c)
  powerOff c (contextAdapter c)
  return ())
