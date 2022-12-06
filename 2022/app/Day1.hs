{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sortBy)

main :: IO ()
main = do
    input <- T.readFile "./data/day1.txt"
    let caloriesByElf = sortBy (flip compare) $ sum <$> (map (read @Int . T.unpack . T.strip) <$> T.lines <$> T.splitOn "\n\n" input)
    print $ sum $ take 1 $ caloriesByElf
    print $ sum $ take 3 $ caloriesByElf