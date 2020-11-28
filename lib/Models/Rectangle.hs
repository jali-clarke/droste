{-# LANGUAGE
    DeriveGeneric
#-}

module Models.Rectangle where

import Data.Aeson.Types
import GHC.Generics

data Rectangle = Rectangle {
    topLeft :: (Int, Int),
    topRight :: (Int, Int),
    bottomRight :: (Int, Int),
    bottomLeft :: (Int, Int)
} deriving Generic

instance FromJSON Rectangle
