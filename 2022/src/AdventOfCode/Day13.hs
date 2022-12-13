module AdventOfCode.Day13 where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative ( Alternative((<|>)) ) 
import Data.List (intercalate, sortBy)
import Control.Monad (forM_)
import Data.Maybe (catMaybes, listToMaybe, fromJust)
import AdventOfCode.Types ( Day(..) )

data Value = List [Value] | Item Int deriving (Eq)
instance Show Value where
    show :: Value -> String
    show (Item x) = show x
    show (List xs) = "[" <> intercalate "," (show <$> xs) <> "]"

lexer = P.makeTokenParser haskellDef

item = Item . fromIntegral <$> P.integer lexer
list = List <$> P.brackets lexer (P.commaSep lexer (item <|> list))
pair = (,) <$> list <*> list
pairs = P.many pair

correctOrder :: (Value, Value) -> Maybe Bool
correctOrder (Item l, Item r) = if l == r then Nothing else Just (l < r)
correctOrder (List l, List r) = listToMaybe $ catMaybes $ (correctOrder <$> zip l r) <> [if length l == length r then Nothing else Just (length l < length r)]
correctOrder (Item l, List r) = correctOrder (List [Item l], List r)
correctOrder (List l, Item r) = correctOrder (List l, List [Item r])

compareValues :: Value -> Value -> Ordering
compareValues left right = case correctOrder (left, right) of
    Nothing -> EQ
    (Just False) -> GT
    (Just True) -> LT

data Day13 = Day13 deriving (Show, Read, Eq)
instance Day Day13 where
    partOne :: Day13 -> String -> String
    partOne _ input = show $ sum $ correctIndexes $ fromJust . correctOrder <$> parseResultPairs input
    partTwo :: Day13 -> String -> String
    partTwo _ input = show $ product $ findInclusions $ sortBy compareValues $ partTwoInclusions <> concatMap tupleToList (parseResultPairs input)

partTwoInclusions :: [Value]
partTwoInclusions = [List [List [Item 2]], List [List [Item 6]]]

parseResultPairs :: String -> [(Value, Value)]
parseResultPairs input = fromRight $ P.parse pairs "" input

fromRight :: Show b => Either b a -> a
fromRight (Right x) = x
fromRight (Left x) = error $ show x

correctIndexes :: [Bool] -> [Int]
correctIndexes = zipWith (\i x -> if x then i else 0) [1..]

tupleToList :: (a, a) -> [a]
tupleToList (x,y) = [x,y]

findInclusions :: [Value] -> [Int]
findInclusions = zipWith (\i x -> if x `elem` partTwoInclusions then i else 1) [1..]



    