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
} deriving (Eq, Show)

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
        (kept, left) = createCluster' mean allMeans pixels ([], [])
        cluster = Cluster mean kept

createCluster' :: Centroid -> [Centroid] -> [Pixel] -> ([Pixel], [Pixel]) -> ([Pixel], [Pixel])
createCluster' _ _ [] clusters = clusters
createCluster' mean allMeans (x:xs) (kept, left)
    | closest == mean = createCluster' mean allMeans xs (x:kept, left)
    | otherwise = createCluster' mean allMeans xs (kept, x:left)
    where
        toPlace = colorToCentroid (pixelColor x)
        closest = closestCentroid allMeans toPlace

getClusters :: [Centroid] -> [Pixel] -> [Cluster]
getClusters [] _ = []
getClusters (mean:means) pixels = cluster:getClusters means pixLeft
    where
        (cluster, pixLeft) = createCluster mean (mean:means) pixels

-- getClusters :: [Centroid] -> [Pixel] -> [Cluster]
-- getClusters means pixels = getClusters' means pixels []

-- getClusters' :: [Centroid] -> [Pixel] -> [Cluster] -> [Cluster]
-- getClusters' [] _ clusters = clusters
-- getClusters' (mean:means) pixels clusters = getClusters' means pixLeft (cluster:clusters)
--     where
--         (cluster, pixLeft) = createCluster mean (mean:means) pixels
