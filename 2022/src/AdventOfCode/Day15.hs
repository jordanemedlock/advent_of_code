module AdventOfCode.Day15 where

import AdventOfCode.Types (Day(..))
import AdventOfCode.Grid
import Data.List (sortOn, nub)
import Data.Maybe (mapMaybe)

data Sensor = Sensor { location :: Point, closestBeacon :: Point, dist :: Int } deriving (Show, Eq)

readSensor :: String -> Sensor
readSensor str = case words str of 
    ["Sensor", "at", xAt, yAt, "closest", "beacon", "is", "at", bxAt, byAt] -> 
        let (s, b) = (Pair (parseNum xAt) (parseNum yAt), Pair (parseNum bxAt) (parseNum byAt))
        in Sensor s b (manhattan s b)

    edge -> error $ "No edge cases: "<>show edge

manhattan :: Point -> Point -> Int
manhattan (Pair x1 y1) (Pair x2 y2) = abs (x1 - x2) + abs (y1 - y2)



parseNum :: String -> Int
parseNum = read . takeWhile (not.(`elem` [',',':'])) . drop 2

getBounds :: Int -> Sensor -> Maybe (Int,Int)
getBounds row (Sensor (Pair x y) _ dist) = if absx >= 0 then Just (x - absx, x + absx) else Nothing
    where absx = dist - abs (row - y)

unionBounds :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
unionBounds (l1, r1) (l2, r2) 
    | l1 > l2 = unionBounds (l2, r2) (l1, r1)
    | r1 >= l2 = [(l1, max r1 r2)]
    | otherwise = [(l1, r1), (l2, r2)]

intersectBounds :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersectBounds (l1, r1) (l2, r2) = if l > r then Nothing else Just (l, r)
    where (l, r) = (max l1 l2, min r1 r2)

pairs :: (a1 -> a1 -> a2) -> [a1] -> [a2]
pairs f [x,y] = [f x y]
pairs f (x:y:xs) = f x y : pairs f xs
pairs f [x] = [f x x]
pairs _ [] = []

unionBoundsEnd :: [(Int, Int)] -> [(Int, Int)]
unionBoundsEnd = converge (==) . iterate (nub . concat . pairs unionBounds)

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys
converge _ _ = error "Edge case"

inRange :: Ord a => (a, a) -> a -> Bool
inRange (l,r) x = l <= x && x <= r

numLine :: Int -> [(Int, Int)] -> String
numLine width ranges = concatMap (hash . inRanges . floor . (+left) . (delt*) . fromIntegral) [1..width]
    where 
        left = fromIntegral $ minimum $ fst <$> ranges :: Double
        right = fromIntegral $ maximum $ snd <$> ranges :: Double
        dist = right - left
        delt = dist / fromIntegral width
        inRanges i = any (`inRange` i) ranges
        hash x = if x then "#" else "."

data Day15 = Day15 deriving (Show, Read, Eq)
instance Day Day15 where
    partOne :: Day15 -> String -> String
    partOne _ input = show $ unionBoundsEnd (mapMaybe (getBounds 2000000) $ sortOn (pX.location) sensors)
        where 
            sensors = readSensor <$> lines input
            _numBeacons = length $ filter ((==2000000).pY.closestBeacon) sensors
    partTwo :: Day15 -> String -> String
    partTwo _ input = show $ (x+1) * 4000000 + y
        where 
            [(y, [Just (_, x), Just (_, _)])] = filter ((/=[Just (0,4000000)]).snd) (zip [0..] (getRowBounds <$> [0..4000000]))
            sensors = readSensor <$> lines input
            getRowBounds row = intersectBounds (0, 4000000) <$> unionBoundsEnd (mapMaybe (getBounds row) $ sortOn (pX.location) sensors)

    