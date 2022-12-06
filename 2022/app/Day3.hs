module Main where
import Data.List (intersect)


splitHalf :: [a] -> ([a], [a])
splitHalf x = splitAt (length x `div` 2) x

findCommon :: Eq a => [a] -> [a] -> a
findCommon xs ys = head $ intersect xs ys

findBadge :: [Char] -> [Char] -> [Char] -> Char
findBadge xs ys zs = head $ intersect xs $ intersect ys zs

groupMap :: Show t => (t -> t -> t -> a) -> [t] -> [a]
groupMap f (x:y:z:xs) = f x y z : groupMap f xs
groupMap _ [] = []
groupMap _ _ = error "No more edge cases"

score :: Char -> Int
score x | x `elem` ['a'..'z'] = (fromEnum x - fromEnum 'a') + 1
        | x `elem` ['A'..'Z'] = (fromEnum x - fromEnum 'A') + 27
        | otherwise = error "Edge cases suck"
        
main :: IO ()
main = do
    input <- readFile "./data/day3.txt"

    print $ sum $ score.uncurry findCommon.splitHalf <$> lines input
    print $ sum $ score <$> groupMap findBadge (lines input)