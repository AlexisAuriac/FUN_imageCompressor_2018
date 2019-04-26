module Main where

import System.Exit
import System.Random
import Control.Exception

import Utility
import Params
import Cluster
import Kmeans

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
