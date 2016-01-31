{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler.Device1.RSSI where

import Data.List
import Data.Maybe
import Data.Int

import Database.MySQL.Simple

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.PropertiesChanged
import DryR.DBus.Util
import DryR.SQL.Query

rssiHandler pC c = do
  let oP = pCObjectPath pC
  let mac = fromJust $ objectPathToMac oP

  let db = contextDatabase c

  let cP = pCChangedProperties pC
  case ((lookup (toVariant ("RSSI" :: MemberName)) $ dictionaryItems cP) >>= fromVariant >>= fromVariant) of
    Just (rssi :: Int16) -> (execute db (getQuery UpdateDeviceRSSI $ contextQueries c) (rssi, mac)) >> return ()
    Nothing -> return ()

  let iP = pCInvalidatedProperties pC
  case (elem "RSSI" iP) of
    True -> (execute db (getQuery UpdateDeviceStatus $ contextQueries c) (2 :: Integer, mac)) >> return ()
    False -> return ()
