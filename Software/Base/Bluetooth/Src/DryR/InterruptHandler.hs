module DryR.InterruptHandler (waitForInterrupt) where

import Control.Concurrent.MVar
import System.Posix.Signals

waitForInterrupt :: IO ()
waitForInterrupt = do
  mvar <- newEmptyMVar

  installHandler sigINT (CatchOnce $ handleInterrupt mvar) Nothing

  takeMVar mvar

handleInterrupt :: MVar () -> IO ()
handleInterrupt mvar = do
  print "Stopping"
  putMVar mvar ()
