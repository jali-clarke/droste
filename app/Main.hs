module Main where

import Codec.Picture

import Shrinker

fileName :: FilePath
fileName = "test.png"

rect :: Rectangle
rect = mkTargetRectangle (989, 877) (989, 1344) (31, 1340) (28, 879)

main :: IO ()
main = do
    maybeImage <- readImage fileName
    case maybeImage of
        Left err -> putStrLn err
        Right dynamicImage ->
            let drosteImage = dynamicPixelMap (shrink rect) dynamicImage
            in savePngImage "test_droste.png" drosteImage
