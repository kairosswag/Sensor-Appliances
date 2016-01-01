module DryR.Main where

import DBus.Client
import DryR.DBus.SignalHandler
import DryR.InterruptHandler

defaultMain = do
  client <- connectSystem
  handlers <- registerHandlers client

  waitForInterrupt

  unregisterHandlers client handlers
  disconnect client
