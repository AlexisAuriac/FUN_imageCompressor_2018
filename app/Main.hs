module Main where

import System.Environment
import System.Exit

import Lib

exitUsage :: [String] -> IO ()
exitUsage argv = if length argv == 1 && argv !! 0 == "-h"
    then do
        putStrLn "USAGE"
        putStrLn "\t./imageCompressor n e IN"
        putStrLn ""
        putStrLn "DESCRIPTION"
        putStrLn "\tn\tnumber of colors in the final image"
        putStrLn "\te\tconvergence limit"
        putStrLn "\tIN\tpath to the file containing the colors of the pixels"
        exitSuccess
    else return ()

parseArgv :: [String] -> IO ()
parseArgv argv = print argv

main :: IO ()
main = do
    argv <- getArgs
    exitUsage argv
