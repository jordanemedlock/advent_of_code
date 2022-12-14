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
    input5 <- readFile "./data/day5_2.txt"
    input6 <- readFile "./data/day6_2.txt"
    input7 <- readFile "./data/day7_2.txt"
    input8 <- readFile "./data/day8_2.txt"
    input9 <- readFile "./data/day9_2.txt"
    input10 <- readFile "./data/day10_2.txt"
    input11 <- readFile "./data/day11_2.txt"
    input12 <- readFile "./data/day12_2.txt"
    input13 <- readFile "./data/day13_2.txt"
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
        describe "Day5" $ do
            it "partOne" $ do
                partOne Day5 input5 `shouldBe` "CMZ"
            it "partTwo" $ do
                partTwo Day5 input5 `shouldBe` "MCD"
        describe "Day6" $ do
            it "partOne" $ do
                partOne Day6 input6 `shouldBe` "7"
            it "partTwo" $ do
                partTwo Day6 input6 `shouldBe` "19"
        describe "Day7" $ do
            it "partOne" $ do
                partOne Day7 input7 `shouldBe` "95437"
            it "partTwo" $ do
                partTwo Day7 input7 `shouldBe` "24933642"
        describe "Day8" $ do
            it "partOne" $ do
                partOne Day8 input8 `shouldBe` "21"
            it "partTwo" $ do
                partTwo Day8 input8 `shouldBe` "8"
        describe "Day9" $ do
            it "partOne" $ do
                partOne Day9 input9 `shouldBe` "13"
            it "partTwo" $ do
                partTwo Day9 input9 `shouldBe` "1"
        describe "Day10" $ do
            it "partOne" $ do
                partOne Day10 input10 `shouldBe` "13140"
            it "partTwo" $ do
                partTwo Day10 input10 `shouldBe` "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######.....\n"
        describe "Day11" $ do
            it "partOne" $ do
                partOne Day11 input11 `shouldBe` "10605"
            it "partTwo" $ do
                partTwo Day11 input11 `shouldBe` "2713310158"       
        describe "Day12" $ do
            it "partOne" $ do
                partOne Day12 input12 `shouldBe` "31"
            it "partTwo" $ do
                partTwo Day12 input12 `shouldBe` "29"
        describe "Day13" $ do
            it "partOne" $ do
                partOne Day13 input13 `shouldBe` "13"
            it "partTwo" $ do
                partTwo Day13 input13 `shouldBe` "140"
