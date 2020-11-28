{-# LANGUAGE
    DeriveGeneric
#-}

module Models.DrosteRequest where

import Data.Aeson.Types
import GHC.Generics

import Models.Rectangle
import Models.StaticPath

data DrosteRequest = DrosteRequest {
    staticPath :: StaticPath,
    rectangle :: Rectangle
} deriving Generic

instance FromJSON DrosteRequest
