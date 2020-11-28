module Shrinker (
    shrink
) where

import Codec.Picture.Types
import Control.Monad.ST

import Models.Rectangle

interpolate1D :: Int -> Int -> Double -> Int
interpolate1D start end t = round $ fromIntegral start + (fromIntegral $ end - start) * t

interpolate2D :: (Int, Int) -> (Int, Int) -> Double -> (Int, Int)
interpolate2D (xStart, yStart) (xEnd, yEnd) t = (interpolate1D xStart xEnd t, interpolate1D yStart yEnd t)

shrinkMap :: Image px -> Rectangle -> (Int, Int) -> (Int, Int)
shrinkMap image rect =
    let maxX = fromIntegral $ imageWidth image - 1
        maxY = fromIntegral $ imageHeight image - 1
    in \(x, y) ->
        let scaledX = fromIntegral x / maxX
            scaledY = fromIntegral y / maxY
            topEdgeInterpolated = interpolate2D (topLeft rect) (topRight rect) scaledX
            bottomEdgeInterpolated = interpolate2D (bottomLeft rect) (bottomRight rect) scaledX
        in interpolate2D topEdgeInterpolated bottomEdgeInterpolated scaledY

shrink :: Pixel px => Rectangle -> Image px -> Image px
shrink rect image =
    let mapping = shrinkMap image rect

        folding mutableImage action x y px =
            let (x', y') = mapping (x, y)
            in action *> (writePixel mutableImage x' y' px)
    in runST $ do
        mutableImage <- thawImage image
        pixelFold (folding mutableImage) (pure ()) image
        unsafeFreezeImage mutableImage
