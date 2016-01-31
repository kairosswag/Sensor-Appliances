{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.PropertiesChangedHandler.Device1.Connected (connectedHandler) where

import Control.Concurrent
import Control.Monad.Loops

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

tryStartNotify c oP = do
  m_oP_sv_hum <- getGattService1 c oP sv_hum
  case (m_oP_sv_hum) of
    Just oP_sv_hum -> do
      m_oP_ch_hum <- getGattCharacteristic1 c oP_sv_hum ch_hum
      case (m_oP_ch_hum) of
        Just oP_ch_hum -> startNotify c oP_ch_hum
        Nothing -> return Nothing
    Nothing -> return Nothing

connectedTrue pC c = do
  let oP = pCObjectPath pC
  let mac = objectPathToMac oP

  untilJust (threadDelay 2000000 >> tryStartNotify c oP)

  execute (contextDatabase c) (getQuery UpdateDeviceStatus $ contextQueries c) (1 :: Integer, mac)
  return ()

connectedFalse pC c = do
  let oP = pCObjectPath pC
  let mac = objectPathToMac oP

  execute (contextDatabase c) (getQuery UpdateDeviceStatus $ contextQueries c) (0 :: Integer, mac)
  return ()
