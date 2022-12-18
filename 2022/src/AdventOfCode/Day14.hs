module AdventOfCode.Day14 where

import AdventOfCode.Types (Day(..))
import AdventOfCode.Grid 
import qualified Data.Text as T

type Bounds = (Point, Point)
type Path = [Point]

data Material = Air | Stone | Sand deriving (Eq)
instance Show Material where
    show :: Material -> String
    show Air = "."
    show Stone = "#"
    show Sand = "o"

data OffsetGrid a = OffsetGrid { bounds :: Bounds, offsetGrid :: Grid a }
instance Show a => Show (OffsetGrid a) where
    show :: OffsetGrid a -> String
    show (OffsetGrid (Pair x1 y1, Pair x2 y2) grid) = unlines [ "width: "<>show x1<>" <---> "<>show x2
                                                        , "height: "<>show y1<>" <---> "<>show y2
                                                        , show grid
                                                        ]

data State = State 
    { leftCount :: Int
    , rightCount :: Int
    , overflow :: Bool
    , gridState :: OffsetGrid Material 
    }
instance Show State where
    show :: State -> String
    show (State left right overflow grid) = unlines ["Left: "<>show left<>" revTri: "<>show (revTri left), "Right: "<>show right<>" revTri: "<>show (revTri right), "Overflow: "<>show overflow, show grid]

initOffsetGrid :: a -> Bounds -> OffsetGrid a
initOffsetGrid x bounds = OffsetGrid bounds $ initGrid (boundsDims bounds) x

offsetIndex :: OffsetGrid a -> Point -> a
offsetIndex (OffsetGrid bounds grid) index = indexGrid grid $ toBounds bounds index

stateIndex :: State -> Point -> Material
stateIndex (State left right _ grid) pos@(Pair x y) 
    | inBounds b pos = offsetIndex grid pos
    | isLeft b pos = if y == y1 then Stone else if y1 - revTri left <= y then Sand else Air
    | isRight b pos = if y == y1 then Stone else if y1 - revTri right <= y then Sand else Air
    | otherwise = Air
    where b@(Pair x0 _, Pair x1 y1) = bounds grid
    
isLeft :: Bounds -> Point -> Bool
isLeft (Pair x0 _, _) (Pair x _) = x < x0

isRight :: Bounds -> Point -> Bool
isRight (_, Pair x1 _) (Pair x _) = x > x1

offsetUpdate :: OffsetGrid a -> Point -> (a -> a) -> OffsetGrid a
offsetUpdate (OffsetGrid bounds grid) index f = OffsetGrid bounds $ updateGrid grid (toBounds bounds index) f
        
boundsDims :: Bounds -> Size
boundsDims (Pair x1 y1, Pair x2 y2) = Pair (x2 - x1 + 1) (y2 - y1 + 1)

readPath :: String -> [Point]
readPath str = readPoint <$> T.splitOn " -> " (T.pack str)
    where 
        readPoint pointStr =  (\[x,y] -> Pair x y) $ read . T.unpack <$> T.splitOn "," pointStr

allPathsBounds :: [Path] -> Bounds
allPathsBounds = foldl1 joinBounds . map pathBounds

addBorder :: Int -> Bounds -> Bounds
addBorder n (Pair x1 y1, Pair x2 y2) = (Pair (x1 - n) (y1 - n), Pair (x2 + n) (y2 + n))

pathBounds :: Path -> Bounds
pathBounds = foldl1 joinBounds . map (\x -> (x,x))

joinBounds :: Bounds -> Bounds -> Bounds
joinBounds (Pair x0 y0, Pair x1 y1) (Pair x2 y2, Pair x3 y3) = (Pair (min x0 x2) (min y0 y2), Pair (max x1 x3) (max y1 y3))

toBounds :: Bounds -> Point -> Point
toBounds (Pair x0 y0, _) (Pair x y) = Pair (x-x0) (y-y0)

interpPath :: Path -> Path
interpPath [] = []
interpPath [x] = [x]
interpPath ((Pair x0 y0):(Pair x1 y1):xs) = init (zipWith Pair [x0,x0+dx..x1] [y0,y0+dy..y1]) <> interpPath (Pair x1 y1 : xs)
    where
        dx = case compare x0 x1 of LT -> 1; EQ -> 0; GT -> -1
        dy = case compare y0 y1 of LT -> 1; EQ -> 0; GT -> -1

inBounds :: Bounds -> Point -> Bool
inBounds (Pair x0 y0, Pair x1 y1) (Pair x y) = x0 <= x && x <= x1 && y0 <= y && y <= y1

down :: Point -> Point
down (Pair x y) = Pair x (y+1)
downLeft :: Point -> Point
downLeft (Pair x y) = Pair (x-1) (y+1)
downRight :: Point -> Point
downRight (Pair x y) = Pair (x+1) (y+1)



addSand :: Point -> State -> State
addSand index state@(State left right overflow grid) 
    | canGoDown = sendSandDown
    | canGoLeft = sendSandLeft
    | canGoRight = sendSandRight
    | canStayPut = stayPut
    | otherwise = error $ "I dont think it should be able to get here: "<>show index
    where 
        b = bounds grid
        canGoDown = stateIndex state (down index) == Air -- So can your mom
        canGoLeft = stateIndex state (downLeft index) == Air
        canGoRight = stateIndex state (downRight index) == Air
        canStayPut = stateIndex state index == Air
        sendSandDown = addIfInBounds state (down index) (State left right True grid)
        sendSandLeft = addIfInBounds state (downLeft index) (State (left+1) right True grid)
        sendSandRight = addIfInBounds state (downRight index) (State left (right+1) True grid)
        stayPut
            | inBounds b index = State left right overflow $ offsetUpdate grid index $ const Sand
            | otherwise = error "I don't think this makes sense..."

addIfInBounds :: State -> Point -> State -> State
addIfInBounds state index state'
    | inBounds (bounds $ gridState state) index = addSand index state
    | otherwise = state'

isFull :: Point -> State -> Bool
isFull index state = gridState state `offsetIndex` index /= Air

revTri :: (Integral b, Integral a) => a -> b
revTri n = floor $ (sqrt (fromIntegral $ 8*n + 1) - 1) / 2 -- source: trust me bro

fillGridWithPaths :: Bool -> OffsetGrid Material -> [Path] -> OffsetGrid Material
fillGridWithPaths addFloor grid ipaths = foldr (\p g -> offsetUpdate g p (const Stone)) grid (concat ipaths <> if addFloor then (`Pair` y1) <$> [x0..x1]else [])
    where (Pair x0 _, Pair x1 y1) = bounds grid

data Day14 = Day14 deriving (Show, Read, Eq)
instance Day Day14 where
    partOne :: Day14 -> String -> String
    partOne _ input = show $ subtract 1 $ length $ takeWhile (not.overflow) $ iterate (addSand (Pair 500 0)) $ State 0 0 False grid
        where 
            paths = readPath <$> lines input
            ipaths = interpPath <$> paths
            (Pair x0 y0, Pair x1 y1) = addBorder 2 $ allPathsBounds paths
            grid = fillGridWithPaths False (initOffsetGrid Air (Pair x0 0, Pair x1 y1)) ipaths
            
    partTwo :: Day14 -> String -> String
    partTwo _ input = show $ length $ takeWhile (not.isFull (Pair 500 0)) $ iterate (addSand (Pair 500 0)) $ State 0 0 False grid
        where 
            paths = readPath <$> lines input
            ipaths = interpPath <$> paths
            (Pair x0 y0, Pair x1 y1) = addBorder 2 $ allPathsBounds paths
            grid = fillGridWithPaths True (initOffsetGrid Air (Pair x0 0, Pair x1 y1)) ipaths