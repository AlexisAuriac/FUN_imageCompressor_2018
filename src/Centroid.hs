module Centroid (
    Centroid(..),
    dispCentroid,
    getRandomCentroids,
    colorToCentroid,
    closestCentroid
) where

import Text.Printf
import System.Random

import Color

data Centroid = Centroid {
    centroidX :: Float,
    centroidY :: Float,
    centroidZ :: Float
} deriving (Eq, Show)

dispCentroid :: Centroid -> IO ()
dispCentroid c = do
    printf "(%f, %f, %f)\n" x y z
    where
        x = centroidX c
        y = centroidY c
        z = centroidZ c

randByte :: IO Int
randByte = do
    n <- randomIO :: IO Int
    return $ n `mod` 255

randCentroid :: IO Centroid
randCentroid = do
    r <- randByte
    g <- randByte
    b <- randByte
    return $ Centroid (fromIntegral r) (fromIntegral g) (fromIntegral b)

getRandomCentroids :: Int -> IO [Centroid]
getRandomCentroids n = getRandomCentroids' n []

getRandomCentroids' :: Int -> [Centroid] -> IO [Centroid]
getRandomCentroids' 0 means = return means
getRandomCentroids' n means = do
    mean <- randCentroid
    getRandomCentroids' (n - 1) (mean:means)

colorToCentroid :: Color -> Centroid
colorToCentroid color = Centroid x y z
    where
        x = fromIntegral (colorR color) :: Float
        y = fromIntegral (colorR color) :: Float
        z = fromIntegral (colorR color) :: Float

euclidianDist :: Centroid -> Centroid -> Float
euclidianDist c1 c2 = sqrt (dx + dy + dz)
    where
        dx = (centroidX c1 - centroidX c2)**2
        dy = (centroidY c1 - centroidY c2)**2
        dz = (centroidZ c1 - centroidZ c2)**2

closestCentroid :: [Centroid] -> Centroid -> Centroid
closestCentroid [] _ = error "No line given"
closestCentroid (x:xs) c1 = closestCentroid' xs c1 x

closestCentroid' :: [Centroid] -> Centroid -> Centroid -> Centroid
closestCentroid' [] _ currClosest = currClosest
closestCentroid' (x:xs) c1 currClosest
    | newDist < distClosest = closestCentroid' xs c1 x
    | otherwise = closestCentroid' xs c1 currClosest
    where
        newDist = euclidianDist c1 x
        distClosest = euclidianDist c1 currClosest
