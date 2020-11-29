module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant
import System.Directory
import System.Environment

import Api
import Server

main :: IO ()
main = do
    staticDir <- getEnv "STATIC_DIR"
    serverPort <- getEnv "SERVER_PORT" >>= readIO
    createDirectoryIfMissing False staticDir
    withStdoutLogger $ \logger ->
        let settings = setPort serverPort . setLogger logger $ defaultSettings
        in runSettings settings (serve api (server staticDir))
