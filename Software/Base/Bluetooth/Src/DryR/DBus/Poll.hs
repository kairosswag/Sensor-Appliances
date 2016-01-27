{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.Poll (poll) where

import Control.Concurrent
import Control.Monad

import Data.Int
import Data.List
import Data.Maybe

import Database.MySQL.Simple

import DBus
import DBus.Client
import DBus.Introspection

import DryR.Context
import DryR.DBus.Introspect
import DryR.DBus.Properties
import DryR.DBus.Util
import DryR.SQL.Query

getDeviceRSSI :: Context -> [String] -> IO ([(String, Int16)])
getDeviceRSSI c devices = do
  let device_paths = map (macToObjectPath (contextAdapter c)) devices
  mrssis <- mapM (\device -> get c "org.bluez" device "org.bluez.Device1" "RSSI") device_paths
  return $ catMaybes $ map (\(device, mrssi) -> (mrssi >>= fromVariant >>= (\rssi -> return (device, rssi)))) $ zip devices mrssis

pollFunc :: InnerContext -> IO ()
pollFunc iC = forever $ do
  withContext (\c -> do
    mi <- introspect c "org.bluez" (contextAdapter c)
    case (mi) of
      Just i -> do
        let devices_found = catMaybes $ map (objectPathToMac . objectPath) $ objectChildren i

        qr :: [Only String] <- query_ (contextDatabase c) (getQuery SelectDevice $ contextQueries c)
        let devices_database = map fromOnly qr

        let devices_not_in_database = devices_found \\ devices_database
        let devices_intersect = intersect devices_found devices_database
        let devices_not_found = devices_database \\ devices_found

        device_rssi_intersect <- getDeviceRSSI c devices_intersect
        device_rssi_not_in_database <- getDeviceRSSI c devices_not_in_database

        print device_rssi_intersect
        print device_rssi_not_in_database

        let db = contextDatabase c
        mapM (\(device, rssi) -> execute db (getQuery UpdateDeviceRSSI $ contextQueries c) (rssi, device)) device_rssi_intersect
        mapM (\(device, rssi) -> execute db (getQuery InsertDevice $ contextQueries c) (device, 0 :: Integer, rssi)) device_rssi_not_in_database

        return ()
      Nothing -> return ()) iC
  threadDelay 10000000

poll :: InnerContext -> IO ()
poll iC = (forkIO $ pollFunc iC) >> (return ())
