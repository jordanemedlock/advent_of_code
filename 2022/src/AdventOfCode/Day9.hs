module AdventOfCode.Day9 where

import qualified Data.Vector as V
import Data.Vector ((!), (//))
import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)
import AdventOfCode.Types ( Day(..) )

type Grid = V.Vector (V.Vector Bool)
data State = State { grid :: Grid, rope :: [(Int,Int)] } deriving (Show)

initGrid :: Int -> Grid
initGrid n = V.replicate n $ V.replicate n False

initState :: Int -> State
initState n = State (initGrid 512) (replicate n (256, 256))


updateGrid :: (Int, Int) -> (Bool -> Bool) -> Grid -> Grid
updateGrid (x, y) f grid = grid // [(y, grid ! y // [(x, f $ grid ! y ! x)])]

lineToDir :: String -> [(Int,Int)]
lineToDir str = case words str of
    ["R", n] -> replicate (read n) (1, 0)
    ["L", n] -> replicate (read n) (-1, 0)
    ["U", n] -> replicate (read n) (0, -1)
    ["D", n] -> replicate (read n) (0, 1)
    edge -> error $ "Fuck edge cases: " <> show edge

execInst :: (Int, Int) -> State -> State
execInst (x, y) (State grid ((hx, hy):ts)) = State grid' rope'
    where
        rope' = moveRope ((hx + x, hy + y):ts)
        grid' = updateGrid (last rope') (const True) grid
execInst _ _ = error "Edge case"

moveRope :: [(Int, Int)] -> [(Int, Int)]
moveRope (h':t:ts) = h' : moveRope (t' : ts)
    where 
        t' = moveTail h' t
moveRope [t] = [t]
moveRope [] = []


moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (hx, hy) (tx, ty) = (tx + ndx, ty + ndy)
    where
        (dx, dy) = (hx - tx, hy - ty)
        ndx | dx `elem` [-1..1] && dy `elem` [-1..1] = 0
            | dx == 0 = 0
            | dx > 0 = 1
            | dx < 0 = -1
            | otherwise = error "Edge case"
        ndy | dy `elem` [-1..1] && dx `elem` [-1..1] = 0
            | dy == 0 = 0
            | dy > 0 = 1
            | dy < 0 = -1
            | otherwise = error "Edge case"

drawState :: State -> String
drawState (State grid ((hx, hy):ts)) = unlines $ [ concat [ pickChar x y | x <- [200..358] ] | y <- [172..380] ]
    where
        pickChar x y
            | x == hx && y == hy = "H"
            | isJust mIdx = show (fromJust mIdx)
            | grid ! y ! x = "#"
            | otherwise = "."
            where 
                mIdx = (+1) <$> (x,y) `elemIndex` ts 
drawState _ = error "Edge case"



data Day9 = Day9 deriving (Show, Read, Eq)
instance Day Day9 where
    -- TODO: I keep assuming foldr is a drop in replacement for whatever loop I make
    partOne :: Day9 -> String -> String
    partOne _ input = show $ countVisited $ grid $ foldr execInst (initState 2) $ concatMap lineToDir $ lines input
    partTwo :: Day9 -> String -> String
    partTwo _ input = show $ countVisited $ grid $ foldr execInst (initState 10) $ concatMap lineToDir $ lines input

countVisited :: Grid -> Int
countVisited grid = sum $ V.map (V.sum . V.map fromEnum) grid

