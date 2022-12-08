module Main where

import qualified Data.Vector as V
import Data.Vector ((//),(!))
import Control.Arrow ( Arrow((***), (&&&)) )

data Visibility = Unknown | Visible | Invisible deriving (Show, Eq)
type Grid = V.Vector (V.Vector (Int, Visibility))

intFromChar :: Char -> Int
intFromChar c = fromEnum c - fromEnum '0'

parseGrid :: String -> Grid
parseGrid str = V.fromList $ V.fromList . map ((,Unknown).intFromChar) <$> lines str

updateGrid :: (Int, Int) -> ((Int, Visibility) -> (Int, Visibility)) -> Grid -> Grid
updateGrid (x, y) f grid = grid // [(y, grid ! y // [(x, f $ grid ! y ! x)])]

indexGrid :: Grid -> (Int, Int) -> (Int, Visibility)
indexGrid grid (x, y) = grid ! y ! x

gridWidth :: Grid -> Int
gridWidth = V.length . V.head

gridHeight :: Grid -> Int
gridHeight = V.length

gridDims :: Grid -> (Int, Int)
gridDims = gridWidth &&& gridHeight

makePathway :: Int -> Int -> [(Int,Int)]
makePathway 0 _ = []
makePathway _ 0 = []
makePathway 1 height = zip (repeat 0) [0..height-1]
makePathway width 1 = zip [0..width-1] (repeat 0)
makePathway width height = zip [0..width-1] (repeat 0) 
                        <> zip (repeat $ width-1) [1..height-1] 
                        <> zip [0..width-2] (repeat $ width-1)
                        <> zip (repeat 0) [1..height-2]
                        <> map ((+1)***(+1)) (makePathway (width-2) (height-2))

updateVisibility :: (Int, Int) -> Grid -> Grid
updateVisibility i@(0, _) grid = updateGrid i (\(v, _) -> (v, Visible)) grid
updateVisibility i@(_, 0) grid = updateGrid i (\(v, _) -> (v, Visible)) grid
updateVisibility i@(x, y) grid 
    | y == height-1 = setVisibility Visible
    | x == width-1 = setVisibility Visible
    | otherwise = if left && right && up && down then setVisibility Invisible else setVisibility Visible
        where 
            dims@(width, height) = gridDims grid
            (value, _) = grid ! y ! x
            setVisibility vis = updateGrid i (\(v, _) -> (v, vis)) grid
            anyAbove dir = any ((>=value).fst.indexGrid grid) $ pathToEnd dir dims i
            left = anyAbove (-1,0)
            right = anyAbove (1,0)
            up = anyAbove (0,-1)
            down = anyAbove (0,1)

treeDistance :: Int -> [Int] -> Int
treeDistance value heights = min (length heights) ((1+).length $ takeWhile (<value) heights)

treeScore :: (Int, Int) -> Grid -> Int
treeScore i@(x, y) grid = left * right * up * down -- ((i, value, (leftValues, rightValues, upValues, downValues)), left * right * up * down) -- 
    where 
        value = fst $ indexGrid grid i
        dims@(width, height) = gridDims grid
        left = if x == 0 then 0 else treeDistance value $ valsAtPath grid $ pathToEnd (-1,0) dims i
        right = if x == width-1 then 0 else treeDistance value $ valsAtPath grid $ pathToEnd (1,0) dims i
        up = if y == 0 then 0 else treeDistance value $ valsAtPath grid $ pathToEnd (0,-1) dims i
        down = if y == height-1 then 0 else treeDistance value $ valsAtPath grid $ pathToEnd (0,1) dims i

pathToEnd :: (Int,Int) -> (Int,Int) -> (Int, Int) -> [(Int,Int)]
pathToEnd (dx, dy) (w,h) (x,y) = zip [x+dx,x+dx*2..ex] [y+dy,y+dy*2..ey]
    where 
        ex | dx < 0 = 0 | dx == 0 = x | otherwise = w-1
        ey | dy < 0 = 0 | dy == 0 = y | otherwise = h-1

valsAtPath :: Grid -> [(Int,Int)] -> [Int]
valsAtPath grid = map (fst.indexGrid grid)

main :: IO ()
main = do
    input <- readFile "./data/day8.txt"
    let grid = parseGrid input
    let height = V.length grid
    let width = V.length $ V.head grid
    let pathway = makePathway width height
    print $ length $ filter ((==Visible).snd) (concatMap V.toList $ V.toList (foldr updateVisibility grid pathway))
    print $ maximum $ flip treeScore grid <$> pathway