module DryR.DBus.SignalHandler (registerHandlers, unregisterHandlers) where

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.PropertiesChanged
import DryR.DBus.PropertiesChangedHandler

handleOrgFreedesktopDBusProperties :: Signal -> Context -> IO ()
handleOrgFreedesktopDBusProperties s c = case (parseSignalToPropertiesChanged s) of
  Just pC -> propertiesChangedHandler pC c
  Nothing -> return ()

matchInterfaceOnly interface = matchAny { matchInterface = Just $ interfaceName_ interface }
interfaces = [
  "org.freedesktop.DBus.Properties"]

matchHandlers = map matchInterfaceOnly interfaces
handlers = [
  handleOrgFreedesktopDBusProperties]

zipHandlers = zip matchHandlers handlers

registerHandlers :: InnerContext -> IO (Maybe [SignalHandler])
registerHandlers = withContextAndInnerContext (\c i ->
  mapM (\(match, handler) -> addMatch (contextDBus c) match (\s -> withContext (handler s) i >> return ())) zipHandlers)

unregisterHandlers :: [SignalHandler] -> InnerContext -> IO (Maybe ())
unregisterHandlers signalHandlers = withContextAndInnerContext (\c _ ->
  mapM_ (\signalHandler -> removeMatch (contextDBus c) signalHandler) signalHandlers)
