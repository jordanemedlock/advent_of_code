{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad ( forM_ )
import Data.List (sort)

data Operation = Add { op :: Int } | Mult { op :: Int } | Exp { op :: Int } deriving Show
data Monkey = Monkey 
    { monkeyNum :: Int
    , startingItems :: [Integer]
    , operation :: Operation
    , testDivisibleBy :: Int
    , ifTrue :: Int, ifFalse :: Int
    } deriving Show

newtype State = State { monkeys :: [Monkey] } deriving Show

parseMonkey :: [String] -> Monkey
parseMonkey [monkey, statingItems, operation, test, ifTrue, ifFalse] = 
    Monkey 
        (parseMonkeyLine $ words monkey) 
        (parseStartingItems $ words statingItems) 
        (parseOperation $ words operation) 
        (parseTest $ words test)
        (parseThrowTo $ words ifTrue)
        (parseThrowTo $ words ifFalse)
parseMonkey edge = error $ "No damn edge cases: " <> show edge

parseMonkeyLine :: [String] -> Int
parseMonkeyLine ["Monkey", n] = read @Int $ takeWhile (/=':') n
parseMonkeyLine edge = error $ "No edge cases: " <> show edge

parseStartingItems :: [String] -> [Integer]
parseStartingItems ("Starting":"items:":xs) = read @Integer . takeWhile (/=',') <$> xs
parseStartingItems edge = error $ "No edge cases: " <> show edge

parseOperation :: [String] -> Operation
parseOperation ["Operation:","new","=","old","+",op] = Add (read op)
parseOperation ["Operation:","new","=","old","*","old"] = Exp 2
parseOperation ["Operation:","new","=","old","*",op] = Mult (read op)
parseOperation edge = error $ "Edge case: " <> show edge

parseTest :: [String] -> Int
parseTest ["Test:", "divisible", "by", op] = read @Int op
parseTest edge = error $ "Edge case: " <> show edge

parseThrowTo :: [String] -> Int
parseThrowTo ["If", _, "throw", "to", "monkey", n] = read @Int n
parseThrowTo edge = error $ "Edge case: " <> show edge

runMonkey :: Int -> Bool -> Int -> State -> (Int, State)
runMonkey superMod attn i s@(State monkeys) = (length items,) $ foldl (flip $ runItem superMod attn monkey') state' items
    where 
        (left, monkey:right) = splitAt i monkeys
        state' = State $ left <> (monkey':right)
        monkey' = monkey { startingItems = [] }
        items = startingItems monkey


runOp :: Operation -> Integer -> Integer
runOp (Add x) = (fromIntegral x +)
runOp (Mult x) = (fromIntegral x *)
runOp (Exp x) = \y -> y * y



runItem :: Int -> Bool -> Monkey -> Integer -> State -> State
runItem superMod attn (Monkey _ _ op test ifTrue ifFalse) item (State monkeys) = State monkeys'
    where 
        level = (if attn then (`div` 3) else id) $ op `runOp` item `mod` fromIntegral superMod
        dest = if level `mod` fromIntegral test == 0 then ifTrue else ifFalse
        (left, destMonkey:right) = splitAt dest monkeys
        destMonkey' = destMonkey { startingItems = startingItems destMonkey <> [level]}
        monkeys' = left <> (destMonkey':right)

runRound :: Int -> Bool -> State -> ([Int], State)
runRound superMod attn state@(State monkeys) = foldl runMonkey' (replicate (length monkeys) 0, state) [0..length monkeys-1]
    where
        runMonkey' (counts, state') i = let (count, state'') = runMonkey superMod attn i state' in
                                        let (left, this:right) = splitAt i counts in -- god fucking complicated
                                        let counts' = left <> (this+count : right) in (counts', state'')

main :: IO ()
main = do
    input <- T.readFile "./data/day11.txt"
    let monkeyLines = lines.T.unpack <$> T.splitOn "\n\n" input
    let monkeys = parseMonkey <$> monkeyLines
    let superMod = product $ testDivisibleBy <$> monkeys
    let runRound' attn (cs, state) = let (cs', state') = runRound superMod attn state in (zipWith (+) cs cs', state')
    let states1 = iterate (runRound' True) (replicate (length monkeys) 0, State monkeys)
    let (counts1, _) = states1 !! 20
    print $ product $ take 2 $ reverse $ sort counts1


    let states2 = iterate (runRound' False) (replicate (length monkeys) 0, State monkeys)
    let (counts2, _) = states2 !! 10000
    print $ product $ take 2 $ reverse $ sort counts2
    