module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Servant
import System.Environment

import Api
import Server

-- rect :: Rectangle
-- rect = mkTargetRectangle (1057, 1054) (1929, 1054) (1929, 1926) (1057, 1926)

main :: IO ()
main = do
    staticDir <- getEnv "STATIC_DIR"
    serverPort <- getEnv "SERVER_PORT" >>= readIO
    withStdoutLogger $ \logger ->
        let settings = setPort serverPort . setLogger logger $ defaultSettings
        in runSettings settings (serve api (server staticDir))
