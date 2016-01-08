{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.Main where

import Control.Concurrent

import DBus.Client
import DryR.DBus.Init
import DryR.DBus.SignalHandler
import DryR.InterruptHandler
import DryR.DBus.MethodHandler

import Data.Maybe
import DBus
import DryR.DBus.Properties
import DryR.Context

defaultMain = do
  vcontext <- newContext

  handlers <- registerHandlers vcontext

  initialize

  exportMethods vcontext
  waitForInterrupt
  unexportMethods vcontext

  uninitialize

  threadDelay 250000

  unregisterHandlers (fromJust handlers) vcontext

  context <- takeContext vcontext
  deleteContext $ fromJust context
