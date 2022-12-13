{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day11 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List (sort)
import AdventOfCode.Types ( Day(..) )

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


readMonkeys :: String -> [Monkey]
readMonkeys input = parseMonkey . lines . T.unpack <$> T.splitOn "\n\n" (T.pack input)

superMod :: [Monkey] -> Int
superMod monkeys = product $ testDivisibleBy <$> monkeys

runRoundAndCombineCounts :: [Monkey] -> Bool -> ([Int], State) -> ([Int], State)
runRoundAndCombineCounts monkeys attn (cs, state) = (zipWith (+) cs cs', state')
    where 
        (cs', state') = runRound (superMod monkeys) attn state

initCountAndState :: Num a => [Monkey] -> ([a], State)
initCountAndState monkeys = (replicate (length monkeys) 0, State monkeys)

runAllRounds :: Bool -> [Monkey] -> [([Int], State)]
runAllRounds attn monkeys = iterate (runRoundAndCombineCounts monkeys attn) (initCountAndState monkeys)

productOfTheTwoLargestCounts :: ([Int], b) -> Int
productOfTheTwoLargestCounts = product . take 2 . reverse . sort . fst

data Day11 = Day11 deriving (Show, Read, Eq)
instance Day Day11 where
    partOne :: Day11 -> String -> String
    partOne _ = show . productOfTheTwoLargestCounts . (!! 20) . runAllRounds True . readMonkeys
    partTwo :: Day11 -> String -> String
    partTwo _ = show . productOfTheTwoLargestCounts . (!! 10000) . runAllRounds False . readMonkeys

    