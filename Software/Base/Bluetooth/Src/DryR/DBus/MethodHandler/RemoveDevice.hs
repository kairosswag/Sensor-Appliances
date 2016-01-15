{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodHandler.RemoveDevice where

import Database.MySQL.Simple

import DBus
import DBus.Client

import qualified DryR.DBus.MethodCall.Adapter1 as A1
import DryR.DBus.Util
import DryR.SQL.Query

import DryR.Context

removeDevice :: String -> Context -> IO ()
removeDevice mac c = do
  let device = macToObjectPath "/org/bluez/hci0" mac
  A1.removeDevice c "/org/bluez/hci0" device

  execute (contextDatabase c) (getQuery DeleteDevice $ contextQueries c) (Only mac)

  return ()
