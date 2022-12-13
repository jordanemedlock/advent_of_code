module Main where

import Test.Hspec
import AdventOfCode
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    input1 <- readFile "./data/day1_2.txt"
    input2 <- readFile "./data/day2_2.txt"
    input3 <- readFile "./data/day3_2.txt"
    input4 <- readFile "./data/day4_2.txt"
    hspec $ do
        describe "Day1" $ do
            it "partOne" $ do
                partOne Day1 input1 `shouldBe` "24000"
            it "partTwo" $ do
                partTwo Day1 input1 `shouldBe` "45000"
        describe "Day2" $ do
            it "partOne" $ do
                partOne Day2 input2 `shouldBe` "15"
            it "partTwo" $ do
                partTwo Day2 input2 `shouldBe` "12"
        describe "Day3" $ do
            it "partOne" $ do
                partOne Day3 input3 `shouldBe` "157"
            it "partTwo" $ do
                partTwo Day3 input3 `shouldBe` "70"
        describe "Day4" $ do
            it "partOne" $ do
                partOne Day4 input4 `shouldBe` "2"
            it "partTwo" $ do
                partTwo Day4 input4 `shouldBe` "4"
