module DryR.Context where

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Database.MySQL.Simple as DB
import qualified DBus.Client as DS

data Context = Context {
  contextDatabase :: DB.Connection,
  contextDBus :: DS.Client}

type InnerContext = MVar (Maybe Context)

newContext :: IO (InnerContext)
newContext = do
  connDBus <- DS.connectSystem
  connDatabase <- DB.connect DB.defaultConnectInfo {DB.connectDatabase="dryr.base", DB.connectUser="dryr.base"}
  newMVar $ Just $ Context connDatabase connDBus

takeContext :: InnerContext -> IO (Maybe Context)
takeContext vcontext = do
  mcontext <- swapMVar vcontext Nothing
  return mcontext

deleteContext :: Context -> IO ()
deleteContext (Context connDatabase connDBus) = do
  DB.close connDatabase
  DS.disconnect connDBus

withContext :: (Context -> IO ()) -> InnerContext -> IO ()
withContext func vcontext  = do
  mcontext <- takeMVar vcontext

  case (mcontext) of
    Just context -> do
      func context
      putMVar vcontext mcontext
    Nothing -> return ()

withContextAsync :: (Context -> IO ()) -> InnerContext-> IO (ThreadId)
withContextAsync func vcontext = forkIO $ withContext func vcontext
