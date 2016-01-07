module DryR.DBus.PropertiesChangedHandler.GattCharacteristic1.Humidity where

import DryR.DBus.PropertiesChanged

humidityHandler pC c = do
  let oP = pCObjectPath pC
  let cP = pCChangedProperties pC

  print cP
