module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import UtilityTest

-- testTrimPre :: IO ()
-- testTrimPre = hspec $ do
--     describe "Utility.trimPre" $ do
--         it "trims elements at the start of a list" $ do
--             trimPre "" `shouldBe` ("" :: String)

exampleTest :: IO ()
exampleTest = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)

        it "returns the first element of an *arbitrary* list" $
            property $ \x xs -> head (x:xs) == (x :: Int)

        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException

main :: IO ()
main = do
    testIsInt
    testIsUInt
