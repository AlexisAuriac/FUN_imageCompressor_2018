module Cluster (
    Cluster(..),
    dispCluster,
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

initClusters :: [Centroid] -> [Cluster]
initClusters centroids = map initCluster centroids
    where initCluster centroid = Cluster centroid []

findClosestCluster :: Pixel -> [Cluster] -> Int
findClosestCluster pixel (c:clusters) =
    findClosestCluster' pixCentroid centroids 1 0 initDist
    where
        centroids = map clusterCentroid clusters
        pixCentroid = colorToCentroid (pixelColor pixel)
        initDist = euclidianDist pixCentroid (clusterCentroid c)

findClosestCluster' :: Centroid -> [Centroid] -> Int -> Int -> Float -> Int
findClosestCluster' _ [] _ idxBest _ = idxBest
findClosestCluster' pixel (c:centroids) idx idxBest bestDist
    | otherDist < bestDist = findClosestCluster' pixel centroids (idx+1) idx otherDist
    | otherwise = findClosestCluster' pixel centroids (idx+1) idxBest bestDist
    where otherDist = euclidianDist pixel c

addCluster :: Cluster -> Pixel -> Cluster
addCluster cluster new = Cluster centroid (new:pixels)
    where
        centroid = clusterCentroid cluster
        pixels = clusterPixels cluster

replace :: Int -> a -> [a] -> [a]
replace idx new xs = start ++ (new:end)
    where
        start = take idx xs
        end = drop (idx + 1) xs

getClusters :: [Centroid] -> [Pixel] -> [Cluster]
getClusters centroids pixels = getClusters' pixels clusters
    where clusters = initClusters centroids

getClusters' :: [Pixel] -> [Cluster] -> [Cluster]
getClusters' [] clusters = clusters
getClusters' (p:pixels) clusters = getClusters' pixels newClusters
    where
        idxCluster = findClosestCluster p clusters
        updatedCluster = addCluster (clusters !! idxCluster) p
        newClusters = replace idxCluster updatedCluster clusters
