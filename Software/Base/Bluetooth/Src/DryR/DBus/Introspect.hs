{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Introspect where

import Data.Maybe

import DBus
import DBus.Client
import DBus.Introspection

introspect :: Client -> BusName -> ObjectPath -> IO (Maybe Object)
introspect client bN oP = do
  let mc = (methodCall oP "org.freedesktop.DBus.Introspectable" "Introspect") {
    methodCallDestination = Just bN
  }

  r <- call client mc
  case (r) of
    Left _ -> return Nothing
    Right mr -> return $ Just $ fromJust $ parseXML oP $ fromJust $ fromVariant ((methodReturnBody mr)!!0)
