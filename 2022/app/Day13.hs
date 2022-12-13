module Main where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative ( Alternative((<|>)) ) 
import Data.List (intercalate, sortBy)
import Control.Monad (forM_)
import Data.Maybe (catMaybes, listToMaybe, fromJust)

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


main :: IO ()
main = do
    let filename = "./data/day13.txt"
    input <- readFile filename
    let (Right resultPairs) = P.parse pairs filename input
    let corrects = fromJust . correctOrder <$> resultPairs
    let indices = zipWith (\i x -> if x then i else 0) [1..] corrects
    print $ sum indices

    let inclusions = [List [List [Item 2]], List [List [Item 6]]]

    let newList = inclusions <> concatMap (\(l,r) -> [l,r]) resultPairs
    let sortedList = sortBy compareValues newList
    -- mapM_ print sortedList
    let indices' = zipWith (\i x -> if x `elem` inclusions then i else 1) [1..] sortedList
    print $ product indices'


    