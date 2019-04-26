module Point (
    Point(..),
    dispPoint,
    getPoint
) where

import Data.List
import Text.Printf
import Control.Exception

import Utility
import Split

data Point = Point {
    pointX :: Int,
    pointY :: Int
} deriving (Eq, Show)

dispPoint :: Point -> IO ()
dispPoint p = do
    printf "(%d,%d)\n" x y
    where
        x = pointX p
        y = pointY p

getPoint :: String -> Point
getPoint str
    | not (prefixOk && suffixOk && length values == 2) = error $ str ++ ": Invalid point syntax"
    | not valuesOk = error $ str ++ ": Invalid values"
    | otherwise = point
    where
        prefixOk = "(" `isPrefixOf` str
        suffixOk = ")" `isSuffixOf` str
        values = split (init (tail str)) ','
        valuesOk = (isInt (values !! 0))
            && (isInt (values !! 1))
        x = read (values !! 0) :: Int
        y = read (values !! 1) :: Int
        point = Point x y
