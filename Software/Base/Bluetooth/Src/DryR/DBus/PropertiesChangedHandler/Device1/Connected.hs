{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.PropertiesChangedHandler.Device1.Connected (connectedHandler) where

import Control.Concurrent

import Data.Maybe

import Database.MySQL.Simple

import DBus

import DryR.Context
import DryR.DBus.MethodCall.Adapter1
import DryR.DBus.MethodCall.GattCharacteristic1
import DryR.DBus.PropertiesChanged
import DryR.DBus.Util
import DryR.Sensirion
import DryR.SQL.Query

connectedHandler pC c = do
  let cP = pCChangedProperties pC
  case ((lookup (toVariant ("Connected" :: MemberName)) $ dictionaryItems cP) >>= fromVariant >>= fromVariant) of
    Just True -> connectedTrue pC c
    Just False -> connectedFalse pC c
    Nothing -> return ()

connectedTrue pC c = do
  let oP = pCObjectPath pC
  let mac = objectPathToMac oP

  threadDelay 2000000

  oP_sv_hum <- getGattService1 c oP sv_hum
  oP_ch_hum <- getGattCharacteristic1 c (fromJust oP_sv_hum) ch_hum

  startNotify c $ fromJust oP_ch_hum

  execute (contextDatabase c) (getQuery UpdateDevice $ contextQueries c) (1 :: Integer, mac)
  return ()

connectedFalse pC c = do
  let oP = pCObjectPath pC
  let mac = objectPathToMac oP

  execute (contextDatabase c) (getQuery UpdateDevice $ contextQueries c) (0 :: Integer, mac)
  return ()
