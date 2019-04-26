module Main where

import System.Exit
import System.Random
import Control.Exception

import Utility
import Params
import Pixel
import Centroid
import Cluster

-- kmeans :: [Clusters] -> [Pixel] -> Float -> [Clusters]
-- kmeans clusters pixels lim =

imgCompressor :: IO ()
imgCompressor = do
    params <- getParsedArgv
    let start = getClusters (paramsCentroids params) (paramsPixels params)
    -- printList (paramsPixels params) dispPixel
    -- putStrLn $ "nb colors: " ++ (show (paramsNbColors params))
    -- putStrLn $ "conv lim: " ++ (show (paramsConvLim params))
    -- printList (paramsCentroids params) dispCentroid
    printList start dispCluster
    -- where
        -- end = kmeans start (paramsPixels params) (paramsConvLim params)

main :: IO ()
main = do
    res <- try imgCompressor :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
