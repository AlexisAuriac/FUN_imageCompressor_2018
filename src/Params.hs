module Params (
    Params(..),
    getParsedArgv
) where

import System.Environment
import System.Exit

import Control.Exception

import Utility
import Split
import Pixel
import Centroid

data Params = Params {
    paramsPixels :: [Pixel],
    paramsNbColors :: Int,
    paramsConvLim :: Float,
    paramsCentroids :: [Centroid]
}

readFileLines :: String -> IO [String]
readFileLines str = do
    content <- readFile str
    return $ lines content

usage :: String -> String
usage progName = "Usage\n\
    \\t" ++ progName ++ " n e file\n\
    \\n\
    \DESCRIPTION\n\
    \\tn\tnumber of colors in the final image\n\
    \\te\tconvergence limit\n\
    \\tIN\tpath to the file containing the colors of the pixels\n"

getParsedArgv :: IO Params
getParsedArgv = do
    argv <- getArgs
    progName <- getProgName
    getParsedArgv' argv progName
getParsedArgv' argv progName
    | length argv /= 3 = error $ usage progName
    | nbColors <= 0 = error "Invalid number of colors"
    | convLim <= 0 || convLim > 1 = error "Invalid convergence limit"
    | otherwise = do
        fileLines <- readFileLines (argv !! 2)
        initCentroids <- getRandomCentroids nbColors
        return $ Params (map getPixel fileLines) nbColors convLim initCentroids
    where
        nbColors = read (argv !! 0) :: Int
        convLim = read (argv !! 1) :: Float
