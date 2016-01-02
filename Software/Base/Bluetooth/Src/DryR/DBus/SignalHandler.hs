module DryR.DBus.SignalHandler (registerHandlers, unregisterHandlers) where

import DBus
import DBus.Client

import DryR.DBus.PropertiesChanged
import DryR.DBus.PropertiesChangedHandler

handleOrgFreedesktopDBusProperties :: Client -> Signal -> IO ()
handleOrgFreedesktopDBusProperties client s = case (parseSignalToPropertiesChanged s) of
  Just pC -> propertiesChangedHandler pC client
  Nothing -> return ()

matchInterfaceOnly interface = matchAny { matchInterface = Just $ interfaceName_ interface }
interfaces = [
  "org.freedesktop.DBus.Properties"]

matchHandlers = map matchInterfaceOnly interfaces
handlers = [
  handleOrgFreedesktopDBusProperties]

registerHandlers :: Client -> IO ([SignalHandler])
registerHandlers client = mapM (\(match, handler) -> addMatch client match (handler client)) $ zip matchHandlers handlers

unregisterHandlers :: Client -> [SignalHandler] -> IO ()
unregisterHandlers client signalHandlers = mapM_ (\signalHandler -> removeMatch client signalHandler) signalHandlers
