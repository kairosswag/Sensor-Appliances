{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.MethodCall.Properties where

import DBus
import DBus.Client
import Control.Error.Util

import DryR.Context

get :: (IsVariant a) => Context -> BusName -> ObjectPath -> InterfaceName -> MemberName -> IO (Maybe a)
get c bN oP iN mN = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Get")) {
    methodCallDestination = Just bN,
    methodCallBody = [toVariant $ iN, toVariant $ mN]
  }

  mr <- call (contextDBus c) mc
  return $ hush mr >>= (\r -> fromVariant $ (methodReturnBody r)!!0)

set :: (IsVariant a) => Context -> BusName -> ObjectPath -> InterfaceName -> MemberName -> a -> IO (Maybe ())
set c bN oP iN mN value = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Set")) {
    methodCallDestination = Just bN,
    methodCallBody = [toVariant iN, toVariant mN, toVariant $ toVariant value]
  }

  mr <- call (contextDBus c) mc
  return $ fmap (const ()) $ Just $ hush mr
