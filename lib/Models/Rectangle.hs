{-# LANGUAGE
    DeriveGeneric
#-}

module Models.Rectangle where

import Data.Aeson.Types
import GHC.Generics

data Point = Point {
    x :: Int,
    y :: Int
} deriving Generic

instance FromJSON Point

data Rectangle = Rectangle {
    topLeft :: Point,
    topRight :: Point,
    bottomRight :: Point,
    bottomLeft :: Point
} deriving Generic

instance FromJSON Rectangle
