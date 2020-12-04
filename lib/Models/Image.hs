{-# LANGUAGE
    DeriveGeneric
#-}

module Models.Image where

import Data.Aeson.Types
import GHC.Generics

data Image = Image {
    path :: String,
    width :: Int,
    height :: Int
} deriving Generic

instance FromJSON Image
instance ToJSON Image
