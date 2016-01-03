{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Properties where

import DBus
import DBus.Client

get :: Client -> ObjectPath -> InterfaceName -> MemberName -> IO (Maybe Variant)
get client oP iN mN = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Get")) {
    methodCallDestination = Just "org.bluez",
    methodCallBody = [toVariant $ iN, toVariant $ mN]
  }

  mr <- call client mc
  case (mr) of
    Left _ -> return Nothing
    Right mr -> return $ Just $ (methodReturnBody mr)!!0

set :: Client -> ObjectPath -> InterfaceName -> MemberName -> Variant -> IO (Maybe ())
set client oP iN mN value = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Set")) {
    methodCallDestination = Just "org.bluez",
    methodCallBody = [toVariant $ iN, toVariant $ mN, value]
  }

  mr <- call client mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()
