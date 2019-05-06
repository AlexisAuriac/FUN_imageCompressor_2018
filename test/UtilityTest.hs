module UtilityTest (
    testIsInt,
    testIsUInt
) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Utility

testIsInt :: IO ()
testIsInt = hspec $ do
    describe "Utility.isInt" $ do
        it "returns False if the string is empty" $ do
            isInt "" `shouldBe` (False :: Bool)

        it "returns False if the string contains any non-digit characters" $ do
            isInt "abc" `shouldBe` (False :: Bool)

        it "returns True if the string is composed only of digits" $ do
            isInt "123" `shouldBe` (True :: Bool)

        it "returns True if the string is composed only of digits and starts with " $ do
            isInt "-123" `shouldBe` (True :: Bool)

testIsUInt :: IO ()
testIsUInt = hspec $ do
    describe "Utility.isUint" $ do
        it "returns False if the string is empty" $ do
            isUint "" `shouldBe` (False :: Bool)

        it "returns False if the string contains any non-digit characters" $ do
            isUint "abc" `shouldBe` (False :: Bool)

        it "returns True if the string is composed only of digits" $ do
            isUint "123" `shouldBe` (True :: Bool)

        it "returns False if the string is composed only of digits and starts with " $ do
            isUint "-123" `shouldBe` (False :: Bool)
