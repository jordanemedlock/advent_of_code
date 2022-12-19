module Main where

import AdventOfCode
import System.Environment (getArgs)
import Control.Monad ( (<=<) )

runDayPart :: (Day d, Show d) => d -> FilePath -> Int -> IO String
runDayPart d filename 1 = partOne d <$> readFile filename
runDayPart d filename 2 = partTwo d <$> readFile filename
runDayPart d filename _ = error "only two parts per day"

dayFuncs :: [String -> Int -> IO String]
dayFuncs = [ runDayPart Day1
           , runDayPart Day2
           , runDayPart Day3
           , runDayPart Day4
           , runDayPart Day5
           , runDayPart Day6
           , runDayPart Day7
           , runDayPart Day8
           , runDayPart Day9
           , runDayPart Day10
           , runDayPart Day11
           , runDayPart Day12
           , runDayPart Day13
           , runDayPart Day14
           , runDayPart Day15
           , runDayPart Day16
           ]

runAll :: IO ()
runAll = mapM_ runDayAll [1..length dayFuncs]

runDayAll :: Int -> IO ()
runDayAll day = mapM_ (runDay day) [1,2] >> mapM_ (runDayTest day) [1,2]

runDay :: Int -> Int -> IO ()
runDay day part = do
    putStrLn $ "Running day: " <> show day <> " part: " <> show part
    let filename = "./data/day"<>show day<>".txt"
    res <- (dayFuncs !! (day - 1)) filename part
    putStrLn res

runDayTest :: Int -> Int -> IO ()
runDayTest day part = do
    putStrLn $ "Running test day: " <> show day <> " part: " <> show part
    let filename = "./data/day"<>show day<>"_2.txt"
    res <- (dayFuncs !! (day - 1)) filename part
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
        [day, part, "test"] -> runDayTest (read day) (read part) 
        edge -> error $ "Invalid inputs: " <> show edge