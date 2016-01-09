{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.PropertiesChangedHandler.Device1.Connected (connectedHandler) where

import Data.Maybe

import Database.MySQL.Simple

import DBus

import DryR.Context
import DryR.DBus.PropertiesChanged
import DryR.DBus.Util
import DryR.SQL.Query

connectedHandler pC c = do
  let cP = pCChangedProperties pC
  case ((lookup (toVariant ("Connected" :: MemberName)) $ dictionaryItems cP) >>= fromVariant >>= fromVariant) of
    Just True -> connectedTrue pC c
    Just False -> connectedFalse pC c
    Nothing -> return ()

connectedTrue pC c = do
  let mac = objectPathToMac $ pCObjectPath pC
  execute (contextDatabase c) (getQuery UpdateDevice $ contextQueries c) (1 :: Integer, mac)
  return ()

connectedFalse pC c = do
  let mac = objectPathToMac $ pCObjectPath pC
  execute (contextDatabase c) (getQuery UpdateDevice $ contextQueries c) (2 :: Integer, mac)
  return ()
