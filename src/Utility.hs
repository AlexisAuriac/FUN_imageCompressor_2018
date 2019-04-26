module Utility (
    trimPre,
    trimSuf,
    trim,
    allElemsOf,
    isInt,
    isUint,
    printList,
    average
) where

trimPre :: (Eq a) => [a] -> a -> [a]
trimPre [] _ = []
trimPre (x:xs) c
    | x == c = trimPre xs c
    | otherwise = x:xs

trimSuf :: (Eq a) => [a] -> a -> [a]
trimSuf xs c = reverse $ trimPre (reverse xs) c

trim :: (Eq a) => [a] -> a -> [a]
trim xs c = trimSuf (trimPre xs c) c

allElemsOf :: (Eq a) => [a] -> [a] -> Bool
allElemsOf [] _ = True
allElemsOf (c:xs) src = c `elem` src && (allElemsOf xs src)

isUint :: String -> Bool
isUint "" = False
isUint xs = allElemsOf xs "0123456789"

isInt :: String -> Bool
isInt "" = False
isInt (x:xs)
    | x == '-' = allElemsOf xs digits
    | otherwise = allElemsOf (x:xs) digits
    where digits = "0123456789"

printList :: (Show a) => [a] -> (a -> IO ()) -> IO ()
printList [] _ = return ()
printList (x:xs) f = do
    f x
    printList xs f

average :: [Int] -> Float
average arr = sumArr / len
    where
        sumArr = fromIntegral (sum arr) :: Float
        len = fromIntegral (length arr) :: Float
