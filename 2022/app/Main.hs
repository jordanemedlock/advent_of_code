module Main where

import AdventOfCode
import System.Environment (getArgs)
import Control.Monad ( (<=<) )

runDayPart :: (Day d, Show d) => d -> FilePath -> Int -> IO String
runDayPart d filename 1 = partOne d <$> readFile filename
runDayPart d filename 2 = partTwo d <$> readFile filename
runDayPart d filename _ = error "only two parts per day"

dayFuncs :: [Int -> IO String]
dayFuncs = [ runDayPart Day1 "./data/day1.txt"
           , runDayPart Day2 "./data/day2.txt"
           , runDayPart Day3 "./data/day3.txt"
           , runDayPart Day4 "./data/day4.txt"
           , runDayPart Day5 "./data/day5.txt"
           , runDayPart Day6 "./data/day6.txt"
           , runDayPart Day7 "./data/day7.txt"
           , runDayPart Day8 "./data/day8.txt"
           , runDayPart Day9 "./data/day9.txt"
           , runDayPart Day10 "./data/day10.txt"
           , runDayPart Day11 "./data/day11.txt"
           , runDayPart Day12 "./data/day12.txt"
           , runDayPart Day13 "./data/day13.txt"
           , runDayPart Day14 "./data/day14.txt"
           ]

runAll :: IO ()
runAll = mapM_ runDayAll [1..length dayFuncs]

runDayAll :: Int -> IO ()
runDayAll day = mapM_ (runDay day) [1,2]

runDay :: Int -> Int -> IO ()
runDay day part = do
    putStrLn $ "Running day: " <> show day <> " part: " <> show part
    res <- (dayFuncs !! (day - 1)) part
    putStrLn res

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runAll
        ["all"] -> runAll
        [day] -> runDayAll (read day)
        [day, "all"] -> runDayAll (read day)
        [day, part] -> runDay (read day) (read part) 
        edge -> error $ "Invalid inputs: " <> show edge