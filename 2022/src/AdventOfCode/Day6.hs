module AdventOfCode.Day6 where

import AdventOfCode.Types ( Day(..) )
import Data.List (nub)

findSignal :: (Eq a) => Int -> Int -> [a] -> Int
findSignal n i str 
    | length (nub $ take n str) == n = i
    | otherwise = findSignal n (i+1) (tail str)

data Day6 = Day6 deriving (Show, Read, Eq)
instance Day Day6 where
    partOne :: Day6 -> String -> String
    partOne _ input = show $ findSignal 4 4 input
    partTwo :: Day6 -> String -> String
    partTwo _ input = show $ findSignal 14 14 input
