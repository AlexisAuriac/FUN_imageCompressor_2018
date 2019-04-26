module Color (
    Color(..),
    dispColor,
    getColor
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
