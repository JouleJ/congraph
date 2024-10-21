module Core.Image where

import Core.Algebra
import Core.Color
import Core.Metric
import Core.Space

data Image = Image (Point2D -> Color)

staticColor :: Color -> Image
staticColor c = Image (\_ -> c)

makeGrid :: Int -> Int -> [Point2D]
makeGrid n m = [Point (Vector2D (fromIntegral i / fromIntegral n) (fromIntegral j / fromIntegral m)) | i <- [0..n], j <- [0..m]]

makeImageFromPixels :: [(Point2D, Color)] -> Image
makeImageFromPixels pixels = Image f
    where f :: Point2D -> Color
          f p = findMinFoldable (map (\(q, c) -> ((distance p q), c)) pixels)

getPixelColor :: Image -> Point2D -> Color
getPixelColor (Image f) = f

imageToPixelGrid :: Int -> Int -> Image -> [(Point2D, Color)]
imageToPixelGrid n m img = let grid = makeGrid n m in
                           let f = getPixelColor img
                           in map (\p -> (p, f p)) grid

cacheImage :: Int -> Int -> Image -> Image
cacheImage n m = makeImageFromPixels . imageToPixelGrid n m
