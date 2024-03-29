module Centroid (
    Centroid(..),
    dispCentroid,
    getRandomCentroids,
    colorToCentroid,
    euclidianDist
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
    printf "(%.2f, %.2f, %.2f)\n" x y z
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
        y = fromIntegral (colorG color) :: Float
        z = fromIntegral (colorB color) :: Float

euclidianDist :: Centroid -> Centroid -> Float
euclidianDist c1 c2 = sqrt (dx + dy + dz)
    where
        dx = (centroidX c1 - centroidX c2)**2
        dy = (centroidY c1 - centroidY c2)**2
        dz = (centroidZ c1 - centroidZ c2)**2
