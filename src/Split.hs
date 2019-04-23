module Split (
    split
) where

import Utility

getChunk :: (Eq a) => [a] -> a -> ([a], [a])
getChunk [] _ = ([], [])
getChunk (x:xs) c
    | x /= c = let (x2, x3) = getChunk xs c in (x:x2, x3)
    | otherwise = ([],x:xs)

split :: (Eq a) => [a] -> a -> [[a]]
split [] _ = []
split xs c
    | chunk == [] = []
    | otherwise = chunk:split rest c
    where (chunk, rest) = getChunk (trim xs c) c
