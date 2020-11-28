module Main where

import Network.Wai.Handler.Warp
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
    run serverPort (serve api (server staticDir))
