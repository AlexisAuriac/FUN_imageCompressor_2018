module Main where

import System.Exit
import System.Random
import Control.Exception

import Utility
import Params
import Pixel
import Color
import Centroid

imgCompressor :: IO ()
imgCompressor = do
    params <- getParsedArgv
    printList (paramsPixels params) dispPixel
    putStrLn $ "nb colors: " ++ (show (paramsNbColors params))
    putStrLn $ "conv lim: " ++ (show (paramsConvLim params))
    printList (paramsCentroids params) dispCentroid

main :: IO ()
main = do
    res <- try imgCompressor :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
