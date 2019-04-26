module Main where

import System.Exit
import System.Random
import Control.Exception

import Utility
import Params
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
averageCentroid cluster = Centroid averageR averageG averageB
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

imgCompressor :: IO ()
imgCompressor = do
    params <- getParsedArgv
    let start = getClusters (paramsCentroids params) (paramsPixels params)
    printList (kmeans start (paramsPixels params) (paramsConvLim params)) dispCluster

main :: IO ()
main = do
    res <- try imgCompressor :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
