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
  UpdateDevice
  deriving (Eq)

queries = [
  DeleteDevice,
  InsertDevice,
  InsertHumidity,
  SelectDevice,
  SelectDeviceConnected,
  UpdateDevice]

names = [
  "DeleteDevice.sql",
  "InsertDevice.sql",
  "InsertHumidity.sql",
  "SelectDevice.sql",
  "SelectDeviceConnected.sql",
  "UpdateDevice.sql"]

getQuery :: QueryIdentifier -> [(QueryIdentifier, String)] -> Query
getQuery qi tb = fromString $ fromJust $ lookup qi tb
