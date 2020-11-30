{-# LANGUAGE
    OverloadedStrings,
    TypeOperators
#-}

module Server (
    server
) where

import Paths_droste

import Codec.Picture hiding (Image)
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Servant
import Servant.Multipart
import qualified Servant.RawM.Server as RawM
import System.FilePath
import System.Directory

import Api
import Models.Image
import Models.DrosteRequest
import Shrinker

type StaticCtx = ReaderT FilePath Handler

staticRoot :: StaticCtx FilePath
staticRoot = ask

relativeToRoot :: FilePath -> StaticCtx FilePath
relativeToRoot relPath = asks $ \root -> joinPath [root, relPath]

saveNewImage :: DynamicImage -> StaticCtx Image
saveNewImage dynamicImage = do
    newFilename <- liftIO $ fmap ((++ ".png") . U.toString) U.nextRandom
    newPath <- relativeToRoot newFilename
    liftIO $ savePngImage newPath dynamicImage
    pure $ Image newFilename

staticServer :: ServerT StaticApi StaticCtx
staticServer = staticRoot >>= RawM.serveDirectoryWebApp

imagesGetAllServer :: ServerT ImagesGetAllApi StaticCtx
imagesGetAllServer = do
    root <- staticRoot
    pathStrings <- liftIO $ listDirectory root
    pure $ fmap Image pathStrings

imagesPostServer :: ServerT ImagesPostApi StaticCtx
imagesPostServer multipartData =
    let dataFiles = files multipartData
    in do
        when (length dataFiles /= 1) $
            throwError $ err400 {errBody = "expected exactly one file to be uploaded"}
        dynamicImage <- case (decodeImage . B.toStrict . fdPayload . head) dataFiles of
            Left err -> throwError $ err400 {errBody = "uploaded file is not a valid image: " <> B.fromString err}
            Right dynamicImage -> pure dynamicImage
        saveNewImage dynamicImage

imagesDeleteServer :: ServerT ImagesDeleteApi StaticCtx
imagesDeleteServer relativePath = do
    imagePath <- relativeToRoot relativePath
    fileExists <- liftIO $ doesFileExist imagePath
    when fileExists $ liftIO $
        removeFile imagePath
    pure NoContent

imagesServer :: ServerT ImagesApi StaticCtx
imagesServer = imagesGetAllServer :<|> imagesPostServer :<|> imagesDeleteServer

drosteServer :: ServerT DrosteApi StaticCtx
drosteServer drosteRequest =
    let relativePath = path . image $ drosteRequest
        transformRect = rectangle drosteRequest
    in do
        imagePath <- relativeToRoot relativePath
        maybeImage <- liftIO $ readImage imagePath
        shrunkImage <- case maybeImage of
            Left err -> throwError $ err500 {errBody = "could not read image: " <> B.fromString err}
            Right newImage -> pure $ dynamicPixelMap (shrink transformRect) newImage
        saveNewImage shrunkImage

landingServer :: ServerT LandingApi StaticCtx
landingServer = pure $ addHeader "/index.html" NoContent

assetsServer :: ServerT AssetsApi StaticCtx
assetsServer = liftIO getDataDir >>= RawM.serveDirectoryWebApp

server :: FilePath -> Server Api
server root = hoistServer api (flip runReaderT root) (staticServer :<|> imagesServer :<|> drosteServer :<|> landingServer :<|> assetsServer)
