{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Util where

import Data.Char
import Data.List

import Data.String.Utils

import DBus

import DryR.Context
import DryR.DBus.MethodCall.Introspectable
import DryR.DBus.MethodCall.Properties

macToObjectPath :: ObjectPath -> String -> ObjectPath
macToObjectPath parent mac = objectPath_ ((formatObjectPath parent) ++ "/dev_" ++ (replace ":" "_" $ map toUpper mac))

objectPathToMac :: ObjectPath -> Maybe String
objectPathToMac oP = fmap (replace "_" ":") $ stripPrefix "dev_" $ reverse $ takeWhile (/= '/') $ reverse $ formatObjectPath oP

parent :: ObjectPath -> ObjectPath
parent "/" = "/"
parent oP = objectPath_ $ reverse $ tail $ dropWhile (/= '/') $ reverse $ formatObjectPath oP

getGattService1 :: Context -> ObjectPath -> String -> IO (Maybe ObjectPath)
getGattService1 c oP uuid = do
  mgs <- get c "org.bluez" oP "org.bluez.Device1" "GattServices"
  case (mgs >>= fromVariant) of
    Just gs -> do
      uuids <- mapM (\g -> do
        uuid <- get c "org.bluez" g "org.bluez.GattService1" "UUID"
        return (uuid >>= fromVariant)) gs
      return $ lookup (Just uuid) $ zip uuids gs
    Nothing -> return Nothing

getGattCharacteristic1 :: Context -> ObjectPath -> String -> IO (Maybe ObjectPath)
getGattCharacteristic1 c oP uuid = do
  mgc <- get c "org.bluez" oP "org.bluez.GattService1" "Characteristics"
  case (mgc >>= fromVariant) of
    Just gc -> do
      uuids <- mapM (\g -> do
        uuid <- get c "org.bluez" g "org.bluez.GattCharacteristic1" "UUID"
        return (uuid >>= fromVariant)) gc
      return $ lookup (Just uuid) $ zip uuids gc
    Nothing -> return Nothing
