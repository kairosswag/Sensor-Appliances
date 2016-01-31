module DryR.DBus.SignalHandler (registerHandlers, unregisterHandlers) where

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.Interfaces
import DryR.DBus.PropertiesChanged
import DryR.DBus.PropertiesChangedHandler

handlePropertiesChanged :: Signal -> Context -> IO ()
handlePropertiesChanged s c = case (parseSignalToPropertiesChanged s) of
  Just pC -> propertiesChangedHandler pC c
  Nothing -> return ()

matchInterfaceAndMember interface member = matchAny {
  matchInterface = Just $ interfaceName_ interface,
  matchMember = Just $ memberName_ member}

matchHandlers = [
  (matchInterfaceAndMember "org.freedesktop.DBus.Properties" "PropertiesChanged", handlePropertiesChanged),
  (matchInterfaceAndMember "org.freedesktop.DBus.ObjectManager" "InterfacesAdded", handleInterfacesAdded),
  (matchInterfaceAndMember "org.freedesktop.DBus.ObjectManager" "InterfacesRemoved", handleInterfacesRemoved)]

registerHandlers :: InnerContext -> IO (Maybe [SignalHandler])
registerHandlers = withContextAndInnerContext (\c i ->
  mapM (\(match, handler) -> addMatch (contextDBus c) match (\s -> withContext (handler s) i >> return ())) matchHandlers)

unregisterHandlers :: [SignalHandler] -> InnerContext -> IO (Maybe ())
unregisterHandlers signalHandlers = withContextAndInnerContext (\c _ ->
  mapM_ (\signalHandler -> removeMatch (contextDBus c) signalHandler) signalHandlers)
