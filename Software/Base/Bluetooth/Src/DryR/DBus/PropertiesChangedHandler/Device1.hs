module DryR.DBus.PropertiesChangedHandler.Device1 where

import DryR.DBus.PropertiesChangedHandler.Device1.Connected
import DryR.DBus.PropertiesChangedHandler.Device1.RSSI

device1Handler pC c = do
  connectedHandler pC c
  rssiHandler pC c
