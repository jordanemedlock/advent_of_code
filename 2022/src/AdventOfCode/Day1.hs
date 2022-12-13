module AdventOfCode.Day1 where

import qualified Data.Text as T
import Data.List (sortBy)
import AdventOfCode.Types ( Day(..) )

data Day1 = Day1 deriving (Show, Read, Eq)
instance Day Day1 where
    partOne :: Day1 -> String -> String
    partOne _ input = show $ sum $ take 1 $ caloriesByElf $ T.pack input
    partTwo :: Day1 -> String -> String
    partTwo _ input = show $ sum $ take 3 $ caloriesByElf $ T.pack input

caloriesByElf :: T.Text -> [Int]
caloriesByElf input = sortBy (flip compare) $ sum <$> (map (read . T.unpack . T.strip) . T.lines <$> T.splitOn "\n\n" input)