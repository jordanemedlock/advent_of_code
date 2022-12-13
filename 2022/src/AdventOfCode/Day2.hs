module AdventOfCode.Day2 where

import AdventOfCode.Types ( Day(..) )

data RPS = Rock | Paper | Scissors deriving (Show, Read, Enum, Eq, Ord)

readRPS :: String -> RPS
readRPS x = case x of 
    "A" -> Rock
    "B" -> Paper
    "C" -> Scissors
    "X" -> Rock
    "Y" -> Paper
    "Z" -> Scissors
    _ -> error "Not dealing with edge cases"

rpsScore :: RPS -> Int
rpsScore x = fromEnum x + 1

winner :: RPS -> RPS
winner x = toEnum $ (fromEnum x + 1) `mod` 3

loser :: RPS -> RPS
loser x = toEnum $ (fromEnum x - 1) `mod` 3

win :: RPS -> RPS -> Int
win op us 
    | winner op == us = 6
    | op == us = 3
    | otherwise = 0

roundScore :: RPS -> RPS -> Int
roundScore op us = win op us + rpsScore us

guessRPS :: RPS -> RPS -> RPS
guessRPS x Rock = loser x
guessRPS x Paper = x
guessRPS x Scissors = winner x

data Day2 = Day2 deriving (Show, Read, Eq)
instance Day Day2 where
    partOne :: Day2 -> String -> String
    partOne _ input = show $ sum $ uncurry roundScore <$> readPlays input
    partTwo :: Day2 -> String -> String
    partTwo _ input = show $ sum $ (\(x,y) -> roundScore x $ guessRPS x y) <$> readPlays input

readPlays :: String -> [(RPS, RPS)]
readPlays input = (\[x,y] -> (x,y)) . map readRPS . words <$> lines input
