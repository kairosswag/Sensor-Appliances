module DryR.SQL.Query where

data Query = DeleteDevice
           | InsertDevice
           | InsertHumidity
           | SelectDevice
           | UpdateDevice

queries = [
  DeleteDevice,
  InsertDevice,
  InsertHumidity,
  SelectDevice,
  UpdateDevice]

names = [
  "DeleteDevice.sql",
  "InsertDevice.sql",
  "InsertHumidity.sql",
  "SelectDevice.sql",
  "UpdateDevice.sql"]
