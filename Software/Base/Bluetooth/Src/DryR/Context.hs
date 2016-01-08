module DryR.Context where

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Database.MySQL.Simple as DB
import qualified DBus.Client as DS

import DryR.SQL.Query
import DryR.SQL.Read

data Context = Context {
  contextDatabase :: DB.Connection,
  contextDBus :: DS.Client,
  contextQueries :: [(QueryIdentifier, String)]}

type InnerContext = MVar (Maybe Context)

newContext :: IO (InnerContext)
newContext = do
  connDBus <- DS.connectSystem
  connDatabase <- DB.connect DB.defaultConnectInfo {DB.connectDatabase="dryr.base", DB.connectUser="dryr.base"}
  querystrings <- readSQL
  newMVar $ Just $ Context connDatabase connDBus querystrings

takeContext :: InnerContext -> IO (Maybe Context)
takeContext vcontext = do
  mcontext <- swapMVar vcontext Nothing
  return mcontext

deleteContext :: Context -> IO ()
deleteContext (Context connDatabase connDBus _) = do
  DB.close connDatabase
  DS.disconnect connDBus

withContext :: (Context -> IO (a)) -> InnerContext -> IO (Maybe a)
withContext func vcontext  = do
  mcontext <- takeMVar vcontext

  case (mcontext) of
    Just context -> do
      ret <- func context
      putMVar vcontext mcontext
      return $ Just ret
    Nothing -> return Nothing

withContextAsync :: (Context -> IO ()) -> InnerContext-> IO (ThreadId)
withContextAsync func vcontext = forkIO $ (withContext func vcontext) >> return ()

withContextAndInnerContext :: (Context -> InnerContext -> IO (a)) -> InnerContext -> IO (Maybe a)
withContextAndInnerContext func icontext = do
  mcontext <- takeMVar icontext

  case (mcontext) of
    Just context -> do
      ret <- func context icontext
      putMVar icontext mcontext
      return $ Just ret
    Nothing -> return Nothing
