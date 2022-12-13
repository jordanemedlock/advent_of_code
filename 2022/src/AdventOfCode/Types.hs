module AdventOfCode.Types where

class Day a where
    partOne :: a -> String -> String
    partTwo :: a -> String -> String