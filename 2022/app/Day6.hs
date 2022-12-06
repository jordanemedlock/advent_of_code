module Main where

import Data.List (nub)

findSignal :: (Eq a) => Int -> Int -> [a] -> (Int, [a])
findSignal n i str 
    | length (nub $ take n str) == n = (i, take n str) 
    | otherwise = findSignal n (i+1) (tail str)

main :: IO ()
main = do
    input <- readFile "./data/day6.txt"
    print $ findSignal 4 4 input
    print $ findSignal 14 14 input