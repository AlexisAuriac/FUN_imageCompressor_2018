module Pixel (
    Pixel(..),
    dispPixel,
    getPixel
) where

import Text.Printf
import Control.Exception

import Split
import Point
import Color

data Pixel = Pixel {
    pixelPoint :: Point,
    pixelColor :: Color
} deriving (Eq, Show)

dispPixel :: Pixel -> IO ()
dispPixel pix = do
    printf "(%d, %d) (%d, %d, %d)\n" x y r g b
    where
        x = pointX $ pixelPoint pix
        y = pointY $ pixelPoint pix
        r = colorR $ pixelColor pix
        g = colorG $ pixelColor pix
        b = colorB $ pixelColor pix

getPixel :: String -> Pixel
getPixel str
    | length values /= 2 = error $ str ++ ": Invalid pixel syntax"
    | otherwise = pix
    where
        values = split str ' '
        p = getPoint (values !! 0)
        c = getColor (values !! 1)
        pix = Pixel p c
