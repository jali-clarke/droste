{-# LANGUAGE
    DataKinds,
    TypeOperators
#-}

module Api where

import Servant
import Servant.Multipart

import Models.DrosteRequest
import Models.StaticPath

type StaticApi = "static" :> Raw
type UploadApi = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] StaticPath
type DrosteApi = "droste" :> ReqBody '[JSON] DrosteRequest :> Post '[JSON] StaticPath

type Api = StaticApi :<|> UploadApi :<|> DrosteApi

staticApi :: Proxy StaticApi
staticApi = Proxy

uploadApi :: Proxy UploadApi
uploadApi = Proxy

drosteApi :: Proxy DrosteApi
drosteApi = Proxy

api :: Proxy Api
api = Proxy
