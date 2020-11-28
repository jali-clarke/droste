{-# LANGUAGE
    DeriveGeneric
#-}

module Models.StaticPath where

import Data.Aeson.Types
import GHC.Generics

data StaticPath = StaticPath {
    path :: String
} deriving Generic

instance FromJSON StaticPath
instance ToJSON StaticPath
