module DryR.DBus.Introspect where

import Data.Maybe

import DBus
import DBus.Client
import DBus.Introspection

introspect :: Client -> ObjectPath -> IO (Object)
introspect client oP = do
  let mc = methodCall oP (interfaceName_ "org.freedesktop.DBus.Introspectable") (memberName_ "Introspect")
  mr <- call_ client mc
  let xml = fromVariant ((methodReturnBody mr)!!0)
  return $ fromJust $ parseXML oP $ fromJust xml
