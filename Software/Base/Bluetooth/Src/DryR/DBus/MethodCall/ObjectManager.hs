{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.MethodCall.ObjectManager where

import Data.Map as M

import Safe

import DBus
import DBus.Client

import DryR.Context

getManagedObjects :: Context -> IO (Maybe (M.Map ObjectPath (M.Map String (M.Map String Variant))))
getManagedObjects c = do
  let mc = (methodCall "/" "org.freedesktop.DBus.ObjectManager" "GetManagedObjects") {
    methodCallDestination = Just "org.bluez"
  }

  mr <- call (contextDBus c) mc
  case (mr) of
    Left _ -> return Nothing
    Right r -> do
      let body = methodReturnBody r
      return $ (headMay body) >>= fromVariant
