module Main where

import Network.Wai.Handler.Warp
import Servant
import System.Environment

import Api
import Server

main :: IO ()
main = do
    staticDir <- getEnv "STATIC_DIR"
    serverPort <- getEnv "SERVER_PORT" >>= readIO
    run serverPort (serve api (server staticDir))
