{-# LANGUAGE
    DeriveGeneric
#-}

module Models.DrosteRequest where

import Data.Aeson.Types
import GHC.Generics

import Models.Rectangle
import Models.Image

data DrosteRequest = DrosteRequest {
    image :: Image,
    rectangle :: Rectangle
} deriving Generic

instance FromJSON DrosteRequest
