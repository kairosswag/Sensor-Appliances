module DryR.SQL.Query where

data Query = DeleteDevice
           | InsertDevice
           | InsertHumidity
           | UpdateDevice

queries = [
  DeleteDevice,
  InsertDevice,
  InsertHumidity,
  UpdateDevice]

names = [
  "DeleteDevice.sql",
  "InsertDevice.sql",
  "InsertHumidity.sql",
  "UpdateDevice.sql"]
