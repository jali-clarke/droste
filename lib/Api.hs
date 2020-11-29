{-# LANGUAGE
    DataKinds,
    TypeOperators
#-}

module Api where

import Servant
import Servant.Multipart

import Models.Image
import Models.DrosteRequest

type StaticApi = "static" :> Raw
type ImagesGetAllApi = Get '[JSON] [Image]
type ImagesPostApi = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Image
type ImagesDeleteApi = Capture "relativePath" String :> DeleteNoContent
type ImagesApi = "images" :> (ImagesGetAllApi :<|> ImagesPostApi :<|> ImagesDeleteApi)
type DrosteApi = "droste" :> ReqBody '[JSON] DrosteRequest :> Post '[JSON] Image

type Api = StaticApi :<|> ImagesApi :<|> DrosteApi

api :: Proxy Api
api = Proxy
