module DryR.SQL.Read where

import System.IO
import System.Environment

import System.FilePath.Posix

import DryR.SQL.Query

import Paths_dryr_base_bluetooth

readSQL :: IO ([(QueryIdentifier, String)])
readSQL = do
  execPath <- getExecutablePath
  let data_paths = map ("Res/SQL" </>) names
  data_filenames <- mapM getDataFileName data_paths
  content <- mapM readFile data_filenames
  return $ zip queries content
