module Params (
    Params,
    paramsPixels,
    paramsNbColors,
    paramsConvLim,
    getParsedArgv
) where

import System.Environment
import System.Exit

import Control.Exception

import Utility
import Split
import Pixel

data Params = Params {
    paramsPixels :: [Pixel],
    paramsNbColors :: Int,
    paramsConvLim :: Float
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
    | otherwise = readFileLines (argv !! 2) >>= (
        \fileLines ->
            return $ Params (map getPixel fileLines) nbColors convLim
        )
    where
        ustr = usage "lol"
        nbColors = read (argv !! 0) :: Int
        convLim = read (argv !! 1) :: Float
