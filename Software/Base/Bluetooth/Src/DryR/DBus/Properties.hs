{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Properties where

import DBus
import DBus.Client

get :: Client -> BusName -> ObjectPath -> InterfaceName -> MemberName -> IO (Maybe Variant)
get client bN oP iN mN = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Get")) {
    methodCallDestination = Just bN,
    methodCallBody = [toVariant $ iN, toVariant $ mN]
  }

  mr <- call client mc
  case (mr) of
    Left _ -> return Nothing
    Right mr -> return $ Just $ (methodReturnBody mr)!!0

set :: Client -> BusName -> ObjectPath -> InterfaceName -> MemberName -> Variant -> IO (Maybe ())
set client bN oP iN mN value = do
  let mc = (methodCall oP ("org.freedesktop.DBus.Properties") ("Set")) {
    methodCallDestination = Just bN,
    methodCallBody = [toVariant $ iN, toVariant $ mN, value]
  }

  mr <- call client mc
  case (mr) of
    Left _ -> return Nothing
    Right _ -> return $ Just ()
