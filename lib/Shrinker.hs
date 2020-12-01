module Shrinker (
    shrink
) where

import Codec.Picture.Types
import Control.Monad.ST

import Models.Rectangle

interpolate1D :: Int -> Int -> Double -> Int
interpolate1D start end t = round $ fromIntegral start + (fromIntegral $ end - start) * t

interpolate2D :: Point -> Point -> Double -> Point
interpolate2D (Point xStart yStart) (Point xEnd yEnd) t =
    Point (interpolate1D xStart xEnd t) (interpolate1D yStart yEnd t)

shrinkMap :: Image px -> Rectangle -> Point -> Point
shrinkMap image rect =
    let maxX = fromIntegral $ imageWidth image - 1
        maxY = fromIntegral $ imageHeight image - 1
    in \(Point xToMap yToMap) ->
        let scaledX = fromIntegral xToMap / maxX
            scaledY = fromIntegral yToMap / maxY
            topEdgeInterpolated = interpolate2D (topLeft rect) (topRight rect) scaledX
            bottomEdgeInterpolated = interpolate2D (bottomLeft rect) (bottomRight rect) scaledX
        in interpolate2D topEdgeInterpolated bottomEdgeInterpolated scaledY

shrink :: Pixel px => Rectangle -> Image px -> Image px
shrink rect image =
    let mapping = shrinkMap image rect

        folding mutableImage action xToMap yToMap px =
            let (Point x' y') = mapping (Point xToMap yToMap)
            in action *> (writePixel mutableImage x' y' px)
    in runST $ do
        mutableImage <- thawImage image
        pixelFold (folding mutableImage) (pure ()) image
        unsafeFreezeImage mutableImage
