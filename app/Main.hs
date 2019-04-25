module Main where

import System.Exit
import Control.Exception

import Utility
import Params
import Pixel

imgCompressor :: Params -> IO ()
imgCompressor params = do
    printList (paramsPixels params) dispPixel
    putStrLn $ "nb colors: " ++ (show (paramsNbColors params))
    putStrLn $ "conv lim: " ++ (show (paramsConvLim params))

main :: IO ()
main = do
    params <- getParsedArgv
    res <- try (imgCompressor params) :: IO (Either SomeException ())
    case res of
        Left err -> do
            print $ show err
            exitWith (ExitFailure 84)
        Right _ -> exitSuccess
