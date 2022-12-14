module Main where

import Test.Hspec
import AdventOfCode
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)

data Test = forall a. (Day a, Show a) => 
    Test { day :: a
         , filename :: FilePath
         , partOneSolution :: String
         , partTwoSolution :: String
         }

tests = [ Test Day1 "./data/day1_2.txt" "24000" "45000"
        , Test Day2 "./data/day2_2.txt" "15" "12"
        , Test Day3 "./data/day3_2.txt" "157" "70"
        , Test Day4 "./data/day4_2.txt" "2" "4"
        , Test Day5 "./data/day5_2.txt" "CMZ" "MCD"
        , Test Day6 "./data/day6_2.txt" "7" "19"
        , Test Day7 "./data/day7_2.txt" "95437" "24933642"
        , Test Day8 "./data/day8_2.txt" "21" "8"
        , Test Day9 "./data/day9_2.txt" "13" "1"
        , Test Day10 "./data/day10_2.txt" "13140" "##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######.....\n"
        
        , Test Day11 "./data/day11_2.txt" "10605" "2713310158"
        , Test Day12 "./data/day12_2.txt" "31" "29"
        , Test Day13 "./data/day13_2.txt" "13" "140"

        , Test Day1 "./data/day1.txt" "71924" "210406"
        , Test Day2 "./data/day2.txt" "13221" "13131"
        , Test Day3 "./data/day3.txt" "7553" "2758"
        , Test Day4 "./data/day4.txt" "475" "825"
        , Test Day5 "./data/day5.txt" "VRWBSFZWM" "RBTWJWMCF"
        , Test Day6 "./data/day6.txt" "1757" "2950"
        , Test Day7 "./data/day7.txt" "2061777" "4473403"
        , Test Day8 "./data/day8.txt" "1717" "321975"
        , Test Day9 "./data/day9.txt" "5735" "2478"
        , Test Day10 "./data/day10.txt" "13220" "###..#..#..##..#..#.#..#.###..####.#..#.\n#..#.#..#.#..#.#.#..#..#.#..#.#....#.#..\n#..#.#..#.#..#.##...####.###..###..##...\n###..#..#.####.#.#..#..#.#..#.#....#.#..\n#.#..#..#.#..#.#.#..#..#.#..#.#....#.#..\n#..#..##..#..#.#..#.#..#.###..####.#..#.\n"
        
        , Test Day11 "./data/day11.txt" "120056" "21816744824"
        , Test Day12 "./data/day12.txt" "468" "459"
        , Test Day13 "./data/day13.txt" "5623" "20570"
        ]

main :: IO ()
main = do
    hspec $ do
        forM_ tests $ \(Test d filename p1 p2) -> do
            input <- runIO $ readFile filename
            describe (show d) $ do
                it "partOne" $ do
                    partOne d input `shouldBe` p1
                it "partTwo" $ do
                    partTwo d input `shouldBe` p2
