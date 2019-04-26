module Cluster (
    Cluster(..),
    dispCluster,
    createCluster,
    getClusters
) where

import Text.Printf

import Utility
import Color
import Pixel
import Centroid

data Cluster = Cluster {
    clusterCentroid :: Centroid,
    clusterPixels :: [Pixel]
} deriving (Show)

dispCluster :: Cluster -> IO ()
dispCluster cluster = do
    putStrLn "--"
    dispCentroid $ clusterCentroid cluster
    putStrLn "-"
    printList (clusterPixels cluster) dispPixel

createCluster :: Centroid -> [Centroid] -> [Pixel] -> (Cluster, [Pixel])
createCluster mean _ [] = (Cluster mean [], [])
createCluster mean allMeans pixels = (cluster, left)
    where
        (kept, left) = createCluster' mean allMeans pixels
        cluster = Cluster mean kept

createCluster' :: Centroid -> [Centroid] -> [Pixel] -> ([Pixel], [Pixel])
createCluster' _ _ [] = ([], [])
createCluster' mean allMeans (x:xs)
    | closest == mean = (x:kept, left)
    | otherwise = (kept, x:left)
    where
        toPlace = colorToCentroid (pixelColor x)
        closest = closestCentroid allMeans toPlace
        (kept, left) = createCluster' mean allMeans xs

getClusters :: [Centroid] -> [Pixel] -> [Cluster]
getClusters [] _ = []
getClusters (mean:means) pixels = cluster:getClusters means pixLeft
    where
        (cluster, pixLeft) = createCluster mean (mean:means) pixels