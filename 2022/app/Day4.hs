
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Arrow ((***))
import Data.List (intersect)

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

main :: IO ()
main = do
    input <- T.readFile "./data/day4.txt"

    let pairs = (makeRange *** makeRange) . makeTuple . T.splitOn "," <$> T.lines input
    print $ sum $ fromEnum . uncurry eitherContains <$> pairs
    print $ sum $ fromEnum . uncurry overlaps <$> pairs

