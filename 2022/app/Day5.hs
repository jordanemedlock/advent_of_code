module Main where

import Data.List (transpose)
import Control.Concurrent (threadDelay)

newtype Crate = Crate { unCrate :: Char }
instance Show Crate where
    show (Crate c) = "["<>[c]<>"]"
instance Read Crate where
    readsPrec i ('[':c:']':rest) = [(Crate c, rest)]
    readsPrec _ _ = []

type Column = [Crate]

newtype Cargo = Cargo { unCargo :: [Column] }
instance Show Cargo where
    show :: Cargo -> String
    show (Cargo columns) = unlines $ (concat <$> transpose (leftPad height "    " <$> strCols)) <> [colNums]
        where 
            height = maximum $ length <$> columns
            strCols = map ((<>" ").show) <$> columns :: [[String]]
            colNums = concatMap ((" "<>).(<>"  ").(:[])) $ zipWith const ['1'..] columns

data Instruction = Instruction 
    { count :: Int
    , startIndex :: Int
    , endIndex :: Int
    }
instance Read Instruction where
    readsPrec :: Int -> ReadS Instruction
    readsPrec i str = case words str of
        ["move", cStr, "from", sStr, "to", eStr] -> [(Instruction (read cStr) (read sStr) (read eStr), [])]
        _ -> []
instance Show Instruction where
    show (Instruction n s e) = unwords ["move",show n,"from",show s,"to",show e]
            

leftPad :: Int -> a -> [a] -> [a]
leftPad n x xs = replicate (n - length xs) x <> xs

isRow :: String -> Bool
isRow ('[':_) = True
isRow (' ':' ':_) = True
isRow _ = False

isInst :: String -> Bool
isInst ('m':_) = True
isInst _ = False

groupNum :: Int -> [a] -> [[a]]
groupNum _ [] = []
groupNum n xs = g : groupNum n rest
    where (g, rest) = splitAt n xs

addRowToCargo :: String -> Cargo -> Cargo
addRowToCargo rowStr (Cargo columns) = Cargo $ zipWith addValue (groupNum 4 rowStr) columns
    where 
        addValue (' ':_) column = column
        addValue str crates = read str : crates 

readCargo :: String -> Cargo
readCargo str = foldr addRowToCargo (Cargo (replicate 9 [])) $ takeWhile isRow (lines str)

moveCrates :: Int -> Int -> Int -> Cargo -> Cargo
moveCrates n s e cargo = addCrates e c cargo'
    where
        (c, cargo') = removeCrates n s cargo

addCrates :: Int -> [Crate] -> Cargo -> Cargo
addCrates i c (Cargo columns) = case splitAt (i-1) columns of
    (start, col:end) -> Cargo $ start <> ((c <> col) : end)
    edge -> error $ "Fuck edge cases: " <> show edge
        

removeCrates :: Int -> Int -> Cargo -> ([Crate], Cargo)
removeCrates n i (Cargo columns) = case splitAt (i-1) columns of
    (start, col:end) -> (take n col, Cargo $ start <> (drop n col : end))
    edge -> error $ "Fuck edge cases: " <> show edge

execInstr1 :: Instruction -> Cargo -> Cargo
execInstr1 (Instruction n s e) cargo = iterate (moveCrates 1 s e) cargo !! n

execInstrn :: Instruction -> Cargo -> Cargo
execInstrn (Instruction n s e) = moveCrates n s e 



main :: IO ()
main = do
    input <- readFile "./data/day5.txt"
    let cargo = readCargo input
    print cargo
    let instructions = read @Instruction <$> dropWhile (not.isInst) (lines input)
    loop instructions cargo


loop :: [Instruction] -> Cargo -> IO ()
loop (inst:rest) cargo = do
    let cargo' = execInstrn inst cargo
    putStrLn "\n\n\n"
    print inst
    print cargo'
    loop rest cargo'
-- loop (Instruction n s e:rest) cargo = loop (Instruction 1 s e:Instruction (n-1) s e:rest) cargo
loop [] _ = return ()