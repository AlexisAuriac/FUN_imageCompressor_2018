module Point (
    Point,
    pointX,
    pointY,
    dispPoint,
    getPoint,
    euclidianDist,
    closestPoint
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
    printf "(%d, %d)\n" x y
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

euclidianDist :: Point -> Point -> Float
euclidianDist p1 p2 = sqrt (dx + dy)
    where
        dx = (fromIntegral (pointX p1) - fromIntegral (pointX p2))**2
        dy = (fromIntegral (pointY p1) - fromIntegral (pointY p2))**2

closestPoint :: [Point] -> Point -> Point
closestPoint [] _ = error "No line given"
closestPoint (x:xs) p1 = closestPoint' xs p1 x

closestPoint' :: [Point] -> Point -> Point -> Point
closestPoint' [] _ currClosest = currClosest
closestPoint' (x:xs) p1 currClosest
    | newDist < distClosest = closestPoint' xs p1 x
    | otherwise = closestPoint' xs p1 currClosest
    where
        newDist = euclidianDist p1 x
        distClosest = euclidianDist p1 currClosest
