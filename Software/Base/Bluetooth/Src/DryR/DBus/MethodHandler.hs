{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodHandler where

import Control.Concurrent

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.MethodHandler.ConnectDevice
import DryR.DBus.MethodHandler.DisconnectDevice
import DryR.DBus.MethodHandler.RemoveDevice
methods = [
  ("ConnectDevice", connectDevice),
  ("DisconnectDevice", disconnectDevice),
  ("RemoveDevice", removeDevice)]

exportMethods :: InnerContext -> IO (Maybe ())
exportMethods = withContextAndInnerContext (\c i -> do
  rNR <- requestName (contextDBus c) "dryr.base.bluetooth" [nameDoNotQueue]
  autoMethods <- mapM (\(name, func) -> return $ autoMethod "dryr.base.bluetooth" name (\mac -> (withContextAsync (func mac) i) >> (return ()))) methods
  export (contextDBus c) "/" autoMethods
  return ())

unexportMethods :: InnerContext -> IO (Maybe ())
unexportMethods = withContextAndInnerContext (\c i -> do
  unexport (contextDBus c) "/"
  rNR <- releaseName (contextDBus c) (busName_ "dryr.base.bluetooth")
  return ())
