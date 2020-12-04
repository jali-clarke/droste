module DrosteTypes where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

newtype Image = Image {
    path :: String,
    width :: Int,
    height :: Int
}

newtype Point = Point {
    x :: Int,
    y :: Int
}

newtype Rectangle = Rectangle {
    topLeft :: Point,
    topRight :: Point,
    bottomRight :: Point,
    bottomLeft :: Point
}

newtype DrosteRequest = DrosteRequest {
    path :: String,
    rectangle :: Rectangle
}

derive instance genericImage :: Generic Image _

instance showImage :: Show Image where
    show = genericShow

instance decodeImage :: Decode Image where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeImage :: Encode Image where
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

derive instance genericPoint :: Generic Point _

instance showPoint :: Show Point where
    show = genericShow

instance decodePoint :: Decode Point where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodePoint :: Encode Point where
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

derive instance genericRectangle :: Generic Rectangle _

instance showRectangle :: Show Rectangle where
    show = genericShow

instance decodeRectangle :: Decode Rectangle where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeRectangle :: Encode Rectangle where
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

derive instance genericDrosteRequest :: Generic DrosteRequest _

instance showDrosteRequest :: Show DrosteRequest where
    show = genericShow

instance decodeDrosteRequest :: Decode DrosteRequest where
    decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeDrosteRequest :: Encode DrosteRequest where
    encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
