module DryR.SQL.Query where

import Data.Maybe
import Data.String

import Database.MySQL.Simple

data QueryIdentifier =
  DeleteDevice |
  InsertDeviceOrUpdate |
  InsertHumidity |
  SelectDevice |
  SelectDeviceConnected |
  UpdateDeviceStatus |
  UpdateDeviceRSSI
  deriving (Eq)

queries = [
  DeleteDevice,
  InsertDeviceOrUpdate,
  InsertHumidity,
  SelectDevice,
  SelectDeviceConnected,
  UpdateDeviceStatus,
  UpdateDeviceRSSI]

names = [
  "DeleteDevice.sql",
  "InsertDeviceOrUpdate.sql",
  "InsertHumidity.sql",
  "SelectDevice.sql",
  "SelectDeviceConnected.sql",
  "UpdateDeviceStatus.sql",
  "UpdateDeviceRSSI.sql"]

getQuery :: QueryIdentifier -> [(QueryIdentifier, String)] -> Query
getQuery qi tb = fromString $ fromJust $ lookup qi tb
