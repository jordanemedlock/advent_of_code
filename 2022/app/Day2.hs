module Main where

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

main :: IO ()
main = do
    input <- readFile "./data/day2.txt"
    let plays = map readRPS . words <$> lines input
    print $ sum $ (\[x,y] -> roundScore x y) <$> plays
    print $ sum $ (\[x,y] -> roundScore x $ guessRPS x y) <$> plays
