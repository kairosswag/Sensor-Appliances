{-# LANGUAGE OverloadedStrings #-}

module DryR.Context where

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Database.MySQL.Simple as DB
import qualified DBus as DS
import qualified DBus.Client as DC

import DryR.SQL.Query
import DryR.SQL.Read

data Context = Context {
  contextDatabase :: DB.Connection,
  contextDBus :: DC.Client,
  contextQueries :: [(QueryIdentifier, String)],
  contextAdapter :: DS.ObjectPath}

type InnerContext = MVar (Maybe Context)

newContext :: IO (InnerContext)
newContext = do
  connDBus <- DC.connectSystem
  connDatabase <- DB.connect DB.defaultConnectInfo {DB.connectDatabase="dryr.base", DB.connectUser="dryr.base"}
  querystrings <- readSQL
  newMVar $ Just $ Context connDatabase connDBus querystrings "/org/bluez/hci0"

takeContext :: InnerContext -> IO (Maybe Context)
takeContext vcontext = do
  mcontext <- swapMVar vcontext Nothing
  return mcontext

deleteContext :: Context -> IO ()
deleteContext (Context connDatabase connDBus _ _) = do
  DB.close connDatabase
  DC.disconnect connDBus

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
