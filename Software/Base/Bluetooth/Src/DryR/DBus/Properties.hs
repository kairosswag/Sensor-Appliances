{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Properties where

import DBus
import DBus.Client
import Control.Error.Util

get :: (IsVariant a) => Client -> BusName -> ObjectPath -> InterfaceName -> MemberName -> IO (Maybe a)
get client bN oP iN mN = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Get")) {
    methodCallDestination = Just bN,
    methodCallBody = [toVariant $ iN, toVariant $ mN]
  }

  mr <- call client mc
  return $ hush mr >>= (\r -> fromVariant $ (methodReturnBody r)!!0)

set :: (IsVariant a) => Client -> BusName -> ObjectPath -> InterfaceName -> MemberName -> a -> IO (Maybe ())
set client bN oP iN mN value = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Set")) {
    methodCallDestination = Just bN,
    methodCallBody = [toVariant iN, toVariant mN, toVariant value]
  }

  mr <- call client mc
  return $ fmap (const ()) $ Just $ hush mr
