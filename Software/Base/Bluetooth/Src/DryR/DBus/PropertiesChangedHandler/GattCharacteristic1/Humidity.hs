{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.PropertiesChangedHandler.GattCharacteristic1.Humidity where

import Data.Maybe
import Data.String
import qualified Data.ByteString as BS

import Database.MySQL.Simple

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.PropertiesChanged
import DryR.DBus.Util
import DryR.SQL.Query as SQL
import DryR.Util

humidityHandler pC c = do
  let oP = pCObjectPath pC
  let cP = pCChangedProperties pC
  case ((lookup (toVariant ("Value" :: MemberName)) $ dictionaryItems cP) >>= fromVariant >>= fromVariant) of
    Just lookupValue -> do
      let mac = fromJust $ objectPathToMac $ parent $ parent oP
      let value = fromJust $ word8ListToFloat $ reverse $ BS.unpack lookupValue

      execute (contextDatabase c) (getQuery InsertHumidity $ contextQueries c) (mac, value)

      return ()
    Nothing -> return ()
