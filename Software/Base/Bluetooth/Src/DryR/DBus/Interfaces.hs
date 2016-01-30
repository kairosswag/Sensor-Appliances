{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DryR.DBus.Interfaces (handleInterfacesAdded, handleInterfacesRemoved) where

import Data.Maybe

import DBus
import DBus.Client

import DryR.Context

--parseBody :: Signal -> [(ObjectPath, InterfaceName)]
--parseBody s =
--  where body = signalBody s
--        tuples :: [(ObjectPath, Variant)] = catMaybes $ map fromVariant body

parseSignal s = arr5
  where body = signalBody s
        arr :: [(ObjectPath, Variant)] = catMaybes $ map fromVariant body
        arr2 :: [(ObjectPath, [String])] = map (\(oP, mv) -> (oP, fromJust mv)) $
              filter (\(_, v) -> isJust v) $
              map (\(oP, v) -> (oP, fromVariant v)) arr
        arr3 :: [[(ObjectPath, String)]] = map (\(oP, v) -> map (oP,) v) arr2
        arr4 :: [(ObjectPath, String)] = concat arr3
        arr5 :: [(ObjectPath, InterfaceName)] = map (\(oP, s) -> (oP, interfaceName_ s)) arr4

handleInterfacesAdded :: Signal -> Context -> IO ()
handleInterfacesAdded s c = do
  let out = parseSignal s
  print out

handleInterfacesRemoved :: Signal -> Context -> IO ()
handleInterfacesRemoved s c = do
  let out = parseSignal s
  print out
