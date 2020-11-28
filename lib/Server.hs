{-# LANGUAGE
    OverloadedStrings,
    TypeOperators
#-}

module Server (
    server
) where

import Codec.Picture
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Servant
import Servant.Multipart
import System.FilePath
import System.Directory

import Api
import Models.DrosteRequest
import Models.StaticPath
import Shrinker

type StaticCtx = ReaderT FilePath Handler

staticRoot :: StaticCtx FilePath
staticRoot = ask

relativeToRoot :: FilePath -> StaticCtx FilePath
relativeToRoot relPath = asks $ \root -> joinPath [root, relPath]

saveNewImage :: DynamicImage -> StaticCtx StaticPath
saveNewImage dynamicImage = do
    newFilename <- liftIO $ fmap ((++ ".png") . U.toString) U.nextRandom
    newPath <- relativeToRoot newFilename
    liftIO $ savePngImage newPath dynamicImage
    pure $ StaticPath newFilename

staticServer :: FilePath -> ServerT StaticApi StaticCtx
staticServer root = serveDirectoryWebApp root

uploadGetAllServer :: ServerT UploadGetAllApi StaticCtx
uploadGetAllServer = do
    root <- staticRoot
    pathStrings <- liftIO $ listDirectory root
    pure $ fmap StaticPath pathStrings

uploadPostServer :: ServerT UploadPostApi StaticCtx
uploadPostServer multipartData =
    let dataFiles = files multipartData
    in do
        when (length dataFiles /= 1) $
            throwError $ err400 {errBody = "expected exactly one file to be uploaded"}
        dynamicImage <- case (decodeImage . B.toStrict . fdPayload . head) dataFiles of
            Left err -> throwError $ err400 {errBody = "uploaded file is not a valid image: " <> B.fromString err}
            Right dynamicImage -> pure dynamicImage
        saveNewImage dynamicImage

uploadServer :: ServerT UploadApi StaticCtx
uploadServer = uploadGetAllServer :<|> uploadPostServer

drosteServer :: ServerT DrosteApi StaticCtx
drosteServer drosteRequest =
    let relativePath = path . staticPath $ drosteRequest
        transformRect = rectangle drosteRequest
    in do
        imagePath <- relativeToRoot relativePath
        maybeImage <- liftIO $ readImage imagePath
        shrunkImage <- case maybeImage of
            Left err -> throwError $ err500 {errBody = "could not read image: " <> B.fromString err}
            Right image -> pure $ dynamicPixelMap (shrink transformRect) image
        saveNewImage shrunkImage

server :: FilePath -> Server Api
server root = hoistServer api (flip runReaderT root) (staticServer root :<|> uploadServer :<|> drosteServer)
