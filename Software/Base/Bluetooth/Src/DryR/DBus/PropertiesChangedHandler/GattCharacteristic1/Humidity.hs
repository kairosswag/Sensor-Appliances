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
  print "Hum"
  let oP = pCObjectPath pC
  let cP = pCChangedProperties pC

  let i = head $ filter (\(k,_) -> (fromJust $ fromVariant k) == "Value") (dictionaryItems cP)
  let valueRaw :: BS.ByteString = fromJust $ fromVariant $ fromJust $ fromVariant $ snd i

  let mac = fromJust $ objectPathToMac $ parent $ parent oP
  let value = fromJust $ word8ListToFloat $ reverse $ BS.unpack valueRaw

  print mac
  print value

  res <- execute (contextDatabase c) (getQuery InsertHumidity $ contextQueries c) (mac, value)
  --print res

  --print value
  return ()
