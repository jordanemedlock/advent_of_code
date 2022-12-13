
{-# LANGUAGE OverloadedStrings #-}
module AdventOfCode.Day4 where
import qualified Data.Text as T
import Control.Arrow ((***))
import Data.List (intersect)
import AdventOfCode.Types ( Day(..) )

makeRange :: T.Text -> (Int,Int)
makeRange x = makeTuple $ read . T.unpack <$> T.splitOn "-" x

contains :: (Eq a, Enum a) => (a, a) -> (a, a) -> Bool
contains (a1, a2) (b1, b2) = a1 `elem` range && a2 `elem` range
    where range = [b1..b2]

overlaps :: (Eq a, Enum a) => (a, a) -> (a, a) -> Bool
overlaps (a1, a2) (b1, b2) = not.null $ [a1..a2] `intersect` [b1..b2]

eitherContains :: (Eq a, Enum a) => (a, a) -> (a, a) -> Bool
eitherContains x y = contains x y || contains y x

makeTuple :: Show b => [b] -> (b, b)
makeTuple [x,y] = (x,y)
makeTuple x = error $ show x

data Day4 = Day4 deriving (Show, Read, Eq)
instance Day Day4 where
    partOne :: Day4 -> String -> String
    partOne _ input = show $ sum $ fromEnum . uncurry eitherContains <$> readPairs input
    partTwo :: Day4 -> String -> String
    partTwo _ input = show $ sum $ fromEnum . uncurry overlaps <$> readPairs input

readPairs :: String -> [((Int, Int), (Int, Int))]
readPairs input = (makeRange *** makeRange) . makeTuple . T.splitOn "," <$> T.lines (T.pack input)


