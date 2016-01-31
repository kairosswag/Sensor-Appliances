{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DryR.Main where

import Control.Concurrent

import Data.Maybe

import DBus
import DBus.Client

import DryR.Context
import DryR.DBus.Init
import DryR.DBus.MethodHandler
import DryR.DBus.MethodCall.Properties
import DryR.DBus.SignalHandler
import DryR.InterruptHandler

defaultMain = do
  vcontext <- newContext

  handlers <- registerHandlers vcontext

  initialize vcontext

  exportMethods vcontext
  waitForInterrupt
  unexportMethods vcontext

  uninitialize vcontext

  threadDelay 250000

  unregisterHandlers (fromJust handlers) vcontext

  context <- takeContext vcontext
  deleteContext $ fromJust context
