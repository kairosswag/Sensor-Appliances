module DryR.DBus.PropertiesChanged where

import Data.Maybe

import DBus

data PropertiesChanged = PropertiesChanged  {
  pCObjectPath :: ObjectPath,
  pCInterfaceName :: InterfaceName,
  pCChangedProperties :: Dictionary,
  pCInvalidatedProperties :: [String]}

parseSignalToPropertiesChanged :: Signal -> Maybe PropertiesChanged
parseSignalToPropertiesChanged s =
  if (length $ sb) == 3 && (isJust iN) && (isJust cP) && (isJust iP)
  then Just $ PropertiesChanged (signalPath s) (fromJust iN) (fromJust cP) (fromJust iP)
  else Nothing
  where
    sb = signalBody s
    iN = fromVariant $ sb!!0
    cP = fromVariant $ sb!!1
    iP = fromVariant $ sb!!2
