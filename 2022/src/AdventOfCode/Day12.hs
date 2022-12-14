module AdventOfCode.Day12 where

import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Arrow ( Arrow((&&&),(***)) )
import Data.Graph.AStar ( aStar ) 
import qualified Data.HashSet as HS
import Data.Maybe ( mapMaybe, fromJust ) 
import AdventOfCode.Types ( Day(..) )


type Grid a = V.Vector (V.Vector a)
type Pos = (Int,Int)

indexGrid :: Grid a -> Pos -> a
indexGrid grid (x, y) = grid ! y ! x

gridWidth :: Grid a -> Int
gridWidth = V.length . V.head

gridHeight :: Grid a -> Int
gridHeight = V.length

gridDims :: Grid a -> Pos
gridDims = gridWidth &&& gridHeight

inBounds :: Pos -> Pos -> Bool
inBounds (width, height) (x, y) = 0 <= x && x < width && 0 <= y && y < height

readGrid :: String -> Grid Char
readGrid str = V.fromList $ V.fromList <$> lines str

findStartAndEnd :: Grid Char -> (Pos, Pos)
findStartAndEnd grid = (start, end)
    where
        (width, height) = gridDims grid
        (start:_) = [(x, y) | x <- [0..width-1], y <- [0..height-1], grid `indexGrid` (x, y) == 'S']
        (end:_) = [(x, y) | x <- [0..width-1], y <- [0..height-1], grid `indexGrid` (x, y) == 'E']

findPossibleStarts :: Grid Char -> [Pos]
findPossibleStarts grid = [(x, y) | x <- [0..width-1], y <- [0..height-1], grid `indexGrid` (x, y) == 'a']
    where
        (width, height) = gridDims grid

possibleNeighbors :: Grid Char -> Pos -> [Pos]
possibleNeighbors grid (x,y) = filter notTooHilly $ filter (inBounds (gridDims grid)) $ ((x+)***(y+)) <$> [(0,1),(0,-1),(1,0),(-1,0)]
    where 
        val = grid `indexGrid` (x,y)
        notTooHilly pos | val == 'S' && grid `indexGrid` pos == 'a' = True
                        | val == 'z' && grid `indexGrid` pos == 'E' = True
                        | val == 'S' && grid `indexGrid` pos /= 'a' = False
                        | val /= 'z' && grid `indexGrid` pos == 'E' = False
                        | otherwise = fromEnum (grid `indexGrid` pos) - fromEnum val <= 1



shortestPath :: Grid Char -> Pos -> Pos -> Maybe [Pos]
shortestPath grid start end@(ex, ey) = aStar graph dist heur goal start
    where 
        graph pos = HS.fromList $ possibleNeighbors grid pos
        dist pos1 pos2 = 1
        heur (x,y) = abs (ex-x) + abs (ey-y)
        goal = (==end)


(.&.) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(.&.) f g a = f a && g a


compareBy :: Ord a => (t -> a) -> t -> t -> Ordering
compareBy f a b = f a `compare` f b

data Day12 = Day12 deriving (Show, Read, Eq)
instance Day Day12 where
    partOne :: Day12 -> String -> String
    partOne _ input = show $ length path
        where 
            grid = readGrid input
            (width, height) = gridDims grid
            path = fromJust $ uncurry (shortestPath grid) $ findStartAndEnd grid
    partTwo :: Day12 -> String -> String
    partTwo _ input = show $ minimum $ length <$> paths
        where 
            grid = readGrid input
            (_, end) = findStartAndEnd grid
            paths = mapMaybe (flip (shortestPath grid) end) (findPossibleStarts grid)


