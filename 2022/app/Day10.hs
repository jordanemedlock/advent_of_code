module Main where

import Control.Arrow ( Arrow((&&&)) ) 

data Inst = AddX { val :: Int, cyclesLeft :: Int } | NoOp deriving Show

data State = State { cycleNum :: Int, reg :: Int, inst :: Inst } deriving Show

parseInst :: String -> Inst
parseInst str = case words str of
    ["addx", n] -> AddX (read n) 1
    ["noop"] -> NoOp
    edge -> error $ "Fuck edge cases: " <> show edge

runCode :: [Inst] -> State -> [State]
runCode [] state = [state]
runCode (inst:insts) (State c r (AddX v 0)) = 
    let state' = State (c+1) (r+v) inst in state' : runCode insts state' 
runCode insts (State c r (AddX v n)) = 
    let state' = State (c+1) r (AddX v (n-1)) in state' : runCode insts state'
runCode (inst:insts) (State c r NoOp) = 
    let state' = State (c+1) r inst in state' : runCode insts state'

crtLines :: Int -> [a] -> [[a]]
crtLines n xs = row : crtLines n rest
    where (row, rest) = splitAt n xs

iF :: p -> p -> Bool -> p
iF x y c = if c then x else y

drawLine :: [State] -> String
drawLine states = iF '#' '.' <$>  zipWith (\col state -> reg state-1 <= col && col <= reg state+1) [0..] states

main :: IO ()
main = do
    input <- readFile "./data/day10.txt"
    let insts = parseInst <$> lines input
    let states = runCode insts (State 0 1 NoOp)
    -- Pointless notation FTW
    print $ sum $ uncurry (*) . (reg &&& cycleNum) . (states !!) . subtract 1 <$> [20, 60..220]

    putStrLn $ unlines $ drawLine <$> take 10 (crtLines 40 states)

