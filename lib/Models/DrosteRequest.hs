{-# LANGUAGE
    DeriveGeneric
#-}

module Models.DrosteRequest where

import Data.Aeson.Types
import GHC.Generics

import Models.Rectangle

data DrosteRequest = DrosteRequest {
    path :: String,
    rectangle :: Rectangle
} deriving Generic

instance FromJSON DrosteRequest
