module DryR.SQL.Query where

import Data.Maybe
import Data.String

import Database.MySQL.Simple

data QueryIdentifier =
  DeleteDevice |
  InsertDevice |
  InsertHumidity |
  SelectDevice |
  SelectDeviceConnected |
  UpdateDeviceStatus |
  UpdateDeviceRSSI
  deriving (Eq)

queries = [
  DeleteDevice,
  InsertDevice,
  InsertHumidity,
  SelectDevice,
  SelectDeviceConnected,
  UpdateDeviceStatus,
  UpdateDeviceRSSI]

names = [
  "DeleteDevice.sql",
  "InsertDevice.sql",
  "InsertHumidity.sql",
  "SelectDevice.sql",
  "SelectDeviceConnected.sql",
  "UpdateDeviceStatus.sql",
  "UpdateDeviceRSSI.sql"]

getQuery :: QueryIdentifier -> [(QueryIdentifier, String)] -> Query
getQuery qi tb = fromString $ fromJust $ lookup qi tb
