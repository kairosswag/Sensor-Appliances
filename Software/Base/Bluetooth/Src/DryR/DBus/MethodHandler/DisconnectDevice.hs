{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodHandler.DisconnectDevice where

import DBus
import DBus.Client

import qualified DryR.DBus.MethodCall.Device1 as D1
import DryR.DBus.Util

import DryR.Context

disconnectDevice :: String -> Context -> IO ()
disconnectDevice mac c = do
  let oP = macToObjectPath "/org/bluez/hci0" mac
  D1.disconnect c oP
  return ()
