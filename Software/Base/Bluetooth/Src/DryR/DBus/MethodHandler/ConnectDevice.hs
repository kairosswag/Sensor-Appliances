{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodHandler.ConnectDevice where

import DBus
import DBus.Client

import qualified DryR.DBus.MethodCall.Device1 as D1
import DryR.DBus.Util

import DryR.Context

connectDevice :: String -> Context -> IO ()
connectDevice mac c = do
  let oP = macToObjectPath "/org/bluez/hci0" mac
  D1.connect c oP
  return ()
