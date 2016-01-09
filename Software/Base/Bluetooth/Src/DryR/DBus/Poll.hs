{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.DBus.Poll (poll) where

import Control.Concurrent
import Control.Monad

import Data.List
import Data.Maybe

import Database.MySQL.Simple

import DBus
import DBus.Client
import DBus.Introspection

import DryR.Context
import DryR.DBus.Introspect
import DryR.DBus.Util
import DryR.SQL.Query

pollFunc :: InnerContext -> IO ()
pollFunc iC = forever $ do
  withContext (\c -> do
    mi <- introspect c "org.bluez" "/org/bluez/hci0"
    case (mi) of
      Just i -> do
        let devices = catMaybes $ map (objectPathToMac . objectPath) $ objectChildren i

        qr :: [Only String] <- query_ (contextDatabase c) (getQuery SelectDevice $ contextQueries c)
        let data_devices = map fromOnly qr

        let devices_diff = devices \\ data_devices

        mapM (\d -> execute (contextDatabase c) (getQuery InsertDevice $ contextQueries c) (d, 0 :: Integer)) devices_diff

        return ()
      Nothing -> return ()) iC
  threadDelay 10000000

poll :: InnerContext -> IO ()
poll iC = (forkIO $ pollFunc iC) >> (return ())
