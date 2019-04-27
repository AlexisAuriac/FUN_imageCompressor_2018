module Kmeans (
    kmeans
) where

import Utility
import Pixel
import Color
import Centroid
import Cluster

clusterColorPart :: (Color -> Int) -> Cluster -> [Int]
clusterColorPart access cluster = map getColorByte pixels
    where
        pixels = clusterPixels cluster
        getColorByte pixel = access $ pixelColor pixel

averageCentroid :: Cluster -> Centroid
averageCentroid cluster
    | (clusterPixels cluster) == [] = clusterCentroid cluster
    | otherwise = Centroid averageR averageG averageB
    where
        averageR = average $ clusterColorPart colorR cluster
        averageG = average $ clusterColorPart colorG cluster
        averageB = average $ clusterColorPart colorB cluster

updateCentroids :: [Cluster] -> [Centroid]
updateCentroids clusters = map averageCentroid clusters

updateClusters :: [Cluster] -> [Pixel] -> [Cluster]
updateClusters clusters pixels = getClusters centroids pixels
    where
        centroids = updateCentroids clusters

kmeans :: [Cluster] -> [Pixel] -> Float -> [Cluster]
kmeans clusters pixels convLim
    | newClusters == clusters = clusters
    | otherwise = kmeans newClusters pixels convLim
    where
        newClusters = updateClusters clusters pixels