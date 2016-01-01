module DryR.DBus.SignalHandler (registerHandlers, unregisterHandlers) where

import DBus
import DBus.Client

handleOrgFreedesktopDBusProperties :: Signal -> IO ()
handleOrgFreedesktopDBusProperties signal = do
  print "OrgFreedesktopDBusProperties"

matchInterfaceOnly interface = matchAny { matchInterface = Just $ interfaceName_ interface }
interfaces = [
  "org.freedesktop.DBus.Properties"]

matchHandlers = map matchInterfaceOnly interfaces
handlers = [
  handleOrgFreedesktopDBusProperties]

registerHandlers :: Client -> IO ([SignalHandler])
registerHandlers client = mapM (\(match, handler) -> addMatch client match handler) $ zip matchHandlers handlers

unregisterHandlers :: Client -> [SignalHandler] -> IO ()
unregisterHandlers client signalHandlers = mapM_ (\signalHandler -> removeMatch client signalHandler) signalHandlers
