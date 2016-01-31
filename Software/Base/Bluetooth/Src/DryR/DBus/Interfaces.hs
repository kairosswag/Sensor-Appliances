{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DryR.DBus.Interfaces (handleInterfacesAdded, handleInterfacesRemoved) where

import Data.Maybe
import Data.Int
import qualified Data.Map as M

import Database.MySQL.Simple

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.MethodCall.Properties
import DryR.DBus.Util
import DryR.SQL.Query

parseSignalBodyIA :: [Variant] -> Maybe (ObjectPath, M.Map String (M.Map String Variant))
parseSignalBodyIA (voP:vd:_) = pure (,) <*> (fromVariant voP) <*> (fromVariant vd)
parseSignalBodyIA _ = Nothing

parseSignalBodyIR :: [Variant] -> Maybe (ObjectPath, [String])
parseSignalBodyIR (voP:vd:_) = pure (,) <*> (fromVariant voP) <*> (fromVariant vd)
parseSignalBodyIR _ = Nothing

handleInterfacesAdded :: Signal -> Context -> IO ()
handleInterfacesAdded s c = case ((parseSignalBodyIA.signalBody) s) of
  Just (oP, i) -> case (M.lookup "org.bluez.Device1" i) of
    Just (m) -> case (M.lookup "RSSI" m) of
      Just (mrssi) -> do
        let mac = objectPathToMac oP
        let rssi :: Int16 = (fromJust.fromVariant) mrssi
        execute (contextDatabase c) (getQuery InsertDeviceOrUpdate $ contextQueries c) (mac, rssi, rssi)
        return ()
      Nothing -> return ()
    Nothing -> return ()
  Nothing -> return ()

handleInterfacesRemoved :: Signal -> Context -> IO ()
handleInterfacesRemoved s c = case ((parseSignalBodyIR.signalBody) s) of
  Just (oP, i) -> case (elem "org.bluez.Device1" i) of
    True -> do
        let mac = objectPathToMac oP
        execute (contextDatabase c) (getQuery UpdateDeviceStatus $ contextQueries c) (2 :: Integer, mac)
        return ()
    False -> return ()
  Nothing -> return ()
