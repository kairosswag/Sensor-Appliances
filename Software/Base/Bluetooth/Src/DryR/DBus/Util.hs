{-# LANGUAGE OverloadedStrings #-}

module DryR.DBus.Util where

import Data.Char
import Data.List

import Data.String.Utils

import DBus

macToObjectPath :: ObjectPath -> String -> ObjectPath
macToObjectPath parent mac = objectPath_ ((formatObjectPath parent) ++ "/dev_" ++ (replace ":" "_" $ map toUpper mac))

objectPathToMac :: ObjectPath -> Maybe String
objectPathToMac oP = fmap (replace "_" ":") $ stripPrefix "dev_" $ reverse $ takeWhile (/= '/') $ reverse $ formatObjectPath oP

parent :: ObjectPath -> ObjectPath
parent "/" = "/"
parent oP = objectPath_ $ reverse $ tail $ dropWhile (/= '/') $ reverse $ formatObjectPath oP
