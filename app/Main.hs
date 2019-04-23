module Main where

import System.Environment
import System.Exit

import Control.Exception

import Utility
import Split
import Pixel

readFileLines :: String -> IO [String]
readFileLines str = do
    content <- readFile str
    return $ lines content

imgCompressor :: [String] -> String -> IO ()
imgCompressor (file:[]) _ = do
    fileLines <- readFileLines file
    printList (map getPixel fileLines) dispPixel
imgCompressor _ progName = putStrLn $ "Usage: " ++ progName ++ " file"

main :: IO ()
main = do
    argv <- getArgs
    progName <- getProgName
    res <- try (imgCompressor argv progName) :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess


-- exitUsage :: [String] -> IO ()
-- exitUsage argv = if length argv == 1 && argv !! 0 == "-h"
--     then do
--         putStrLn "USAGE"
--         putStrLn "\t./imgCompressor n e IN"
--         putStrLn ""
--         putStrLn "DESCRIPTION"
--         putStrLn "\tn\tnumber of colors in the final image"
--         putStrLn "\te\tconvergence limit"
--         putStrLn "\tIN\tpath to the file containing the colors of the pixels"
--         exitSuccess
--     else return ()

-- parseArgv :: [String] -> IO ()
-- parseArgv argv = print argv

-- main :: IO ()
-- main = do
--     argv <- getArgs
--     exitUsage argv
