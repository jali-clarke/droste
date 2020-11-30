{-# LANGUAGE
    DataKinds,
    TypeOperators
#-}

module Api where

import Servant
import Servant.Multipart
import Servant.RawM.Server

import Models.Image
import Models.DrosteRequest

type RedirectGet a = Verb 'GET 301 '[PlainText] (Headers '[Header "Location" String] a)

type StaticApi = "static" :> RawM
type ImagesGetAllApi = Get '[JSON] [Image]
type ImagesPostApi = MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Image
type ImagesDeleteApi = Capture "relativePath" String :> DeleteNoContent
type ImagesApi = "images" :> (ImagesGetAllApi :<|> ImagesPostApi :<|> ImagesDeleteApi)
type DrosteApi = "droste" :> ReqBody '[JSON] DrosteRequest :> Post '[JSON] Image
type ProgrammaticApi = "api" :> (ImagesApi :<|> DrosteApi)
type LandingApi = RedirectGet NoContent
type AssetsApi = RawM
type ClientAssetsApi = LandingApi :<|> AssetsApi

type Api = StaticApi :<|> ProgrammaticApi :<|> ClientAssetsApi

api :: Proxy Api
api = Proxy
