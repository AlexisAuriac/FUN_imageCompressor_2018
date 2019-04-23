module Color (
    Color,
    colorR,
    colorG,
    colorB,
    dispColor,
    getColor,
    euclidianDist,
    closestColor
) where

import Data.List
import Text.Printf
import Control.Exception

import Utility
import Split

data Color = Color {
    colorR :: Int,
    colorG :: Int,
    colorB :: Int
} deriving (Eq, Show)

dispColor :: Color -> IO ()
dispColor c = do
    printf "(%d, %d, %d)\n" r g b
    where
        r = colorR c
        g = colorG c
        b = colorB c

getColorByte :: String -> Int
getColorByte str
    | not $ isInt str = error $ str ++ ": Invalid color byte"
    | byte < 0 || byte > 255 = error $ str ++ ": Invalid color byte"
    | otherwise = byte
    where
        byte = read str :: Int

getColor :: String -> Color
getColor str
    | not (prefixOk && suffixOk && length values == 3) = error $ str ++ ": Invalid color syntax"
    | otherwise = color
    where
        prefixOk = "(" `isPrefixOf` str
        suffixOk = ")" `isSuffixOf` str
        values = split (init (tail str)) ','
        r = getColorByte (values !! 0)
        g = getColorByte (values !! 1)
        b = getColorByte (values !! 2)
        color = Color r g b

euclidianDist :: Color -> Color -> Float
euclidianDist c1 c2 = sqrt (dx + dy + dz)
    where
        dx = (fromIntegral (colorR c1) - fromIntegral (colorR c2))**2
        dy = (fromIntegral (colorG c1) - fromIntegral (colorG c2))**2
        dz = (fromIntegral (colorB c1) - fromIntegral (colorB c2))**2

closestColor :: [Color] -> Color -> Color
closestColor [] _ = error "No line given"
closestColor (x:xs) c1 = closestColor' xs c1 x

closestColor' :: [Color] -> Color -> Color -> Color
closestColor' [] _ currClosest = currClosest
closestColor' (x:xs) c1 currClosest
    | newDist < distClosest = closestColor' xs c1 x
    | otherwise = closestColor' xs c1 currClosest
    where
        newDist = euclidianDist c1 x
        distClosest = euclidianDist c1 currClosest
