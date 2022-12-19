module AdventOfCode.Day16 where

import AdventOfCode.Types (Day(..))

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!))
import Data.Graph.AStar (aStar)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import Data.List (permutations, sortOn, nub)
import System.Random (randomR, randoms, random, mkStdGen, RandomGen)
import Control.Arrow ( Arrow((***)) ) 


data Valve = Valve { flowRate :: Int, leadsTo :: [String] } deriving (Show)
data Step = MoveTo String | Open deriving (Show, Eq)

type Path = [Step]
type Valves = HM.HashMap String Valve

type Individual = [String]
type ElIndividual = ([String], [String])
type Population = [Individual]
type ElPopulation = [ElIndividual]

isValidPath :: Valves -> Path -> String -> Bool
isValidPath valves (MoveTo x:ps) start = x `elem` leadsTo (valves ! start) && isValidPath valves ps x
isValidPath valves (Open:ps) start = isValidPath valves ps start
isValidPath _valves [] _start = True

pathScores :: Valves -> String -> Int -> Path -> [(Int, Int)]
pathScores _valves _start 0 _path = []
pathScores valves _start minLeft (MoveTo x:ps) = pathScores valves x (minLeft - 1) ps
pathScores valves start minLeft (Open:ps) = (minLeft - 1, flowRate (valves ! start)) : pathScores valves start (minLeft-1) ps
pathScores _valves _start _minLeft [] = []

scorePath :: Valves -> String -> Int -> Path -> Int
scorePath valves start minLeft path = sum $ uncurry (*) <$> pathScores valves start minLeft path

scorePathEl :: Valves -> String -> Int -> (Path, Path) -> Int
scorePathEl valves start minLeft (path1, path2) = scorePath valves start minLeft path1 + scorePath valves start minLeft path2


readValve :: String -> (String, Valve)
readValve input = case words input of 
    ("Valve":name:"has":"flow":rate:_:_:"to":_:leadsTo) -> (name,) $ Valve (readRate rate) (readLeadsTo leadsTo)
    edge -> error $ "No edge cases: " <> show edge

readRate :: String -> Int
readRate = read . takeWhile (/=';') . drop 5 

readLeadsTo :: [String] -> [String]
readLeadsTo = map $ takeWhile (/=',')

testPath :: [Step]
testPath =  [ MoveTo "DD", Open, MoveTo "CC", MoveTo "BB", Open, MoveTo "AA", MoveTo "II", MoveTo "JJ", Open, MoveTo "II"
            , MoveTo "AA", MoveTo "DD", MoveTo "EE", MoveTo "FF", MoveTo "GG", MoveTo "HH", Open, MoveTo "GG", MoveTo "FF"
            , MoveTo "EE", Open, MoveTo "DD", MoveTo "CC", Open
            ]



findPathBetween :: String -> String -> Valves -> [Step]
findPathBetween start end valves = MoveTo <$> fromJust (aStar neighbors dist heur isGoal start)
    where 
        neighbors n = HS.fromList $ leadsTo (valves ! n)
        dist _ _ = 1
        heur _ = 1
        isGoal s = s == end

-- TODO: make my own randomized weighted mutations function based on heuristics

permutations' :: Valves -> [String] -> [[String]]
permutations' valves vals = map (helper vals) $ randoms . mkStdGen <$> randoms (mkStdGen 3)
    where 
        helper [] _ = []
        helper xs@(_:_) (i:is) = let item = xs !! (i `mod` (length xs)) in item : helper (filter (/=item) xs) is
        duped = concatMap (\x -> replicate (flowRate $ valves ! x) x) vals




allRoutes :: Valves -> [[String]]
allRoutes valves = permutations' valves valves' -- so many permutations.  cant even use a heuristic
    where 
        valves' = fst <$> sortOn (negate.flowRate.snd) (filter ((/=0).flowRate.snd) (HM.toList valves))
        heuristic route = negate $ sum $ zipWith (*) [30,29..] $ flowRate.snd <$> route

expandRoute :: Valves -> [String] -> [Step]
expandRoute valves (x:y:xs) = findPathBetween x y valves <> [Open] <> expandRoute valves (y:xs)
expandRoute valves [x] = []
expandRoute valves [] = []

{- Genetic algorithm steps:
    Create population
    Test then cull population
    mutate populate
    repeat
    
-}

scorePopulation :: Valves -> [Individual] -> [(Int, Individual)]
scorePopulation valves pop = (`zip` pop) $ scorePath valves "AA" 30 . expandRoute valves . ("AA":) <$> pop

scorePopulationEl :: Valves -> [ElIndividual] -> [(Int, ElIndividual)]
scorePopulationEl valves pop = (`zip` pop) $ scorePathEl valves "AA" 26 . onBoth (expandRoute valves . ("AA":)) <$> pop

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (x, y) = (f x, f y)

cullPopulation :: Eq a => Int -> [(Int, a)] -> [a]
cullPopulation n pop = take (length pop) $ cycle $ snd <$> take n (nub $ sortOn (negate.fst) pop)

swapIndices :: Int -> Int -> [a] -> [a]
swapIndices _ _ [] = []
swapIndices _ _ [x] = [x]
swapIndices i j xs 
    | i == j = xs
    | otherwise = left <> (y:mid) <> (x:right)
    where
        (l, r) = (min i j, max i j)
        (left, x:rest) = splitAt l xs
        (mid, y:right) = splitAt (r-l - 1) rest

mutateIndividual :: (RandomGen g) => g -> Individual -> (Individual, g)
mutateIndividual g ind = (swapIndices l r ind, g'')
    where 
        (l, g') = randomR (0, length ind - 1) g
        (r, g'') = randomR (0, length ind - 1) g'

mutateIndividualEl :: RandomGen g => g -> ElIndividual -> (ElIndividual, g)
mutateIndividualEl g (mine, els) 
    | r = ((mine', els'), g''')
    | otherwise = swapItems g' (mine, els)
    where
        (r, g') = random g
        (mine', g'') = mutateIndividual g' mine
        (els', g''') = mutateIndividual g'' els

swapItems :: RandomGen b => b -> ([a], [a]) -> (([a], [a]), b)
swapItems g (mine, els) = ((els', mine'), g')
    where 
        together = mine <> els
        (r, g') = randomR (0, length together) g
        (mine', els') = splitAt r together






mutatePopulation :: (RandomGen g) => (g -> a -> (b, g)) -> g -> [a] -> ([b], g)
mutatePopulation f g [] = ([], g)
mutatePopulation f g (p:ps) = (ind : pop, g'')
    where 
        (ind, g') = f g p
        (pop, g'') = mutatePopulation f g' ps

iterateGenAlg :: RandomGen g => g -> Valves -> Population -> ([Int], g)
iterateGenAlg g valves pop = (maximum (fst <$> scores) : nextScores, g'')
    where 
        scores = scorePopulation valves pop
        (pop', g') = mutatePopulation mutateIndividual g $ cullPopulation 2500 scores
        (nextScores, g'') = iterateGenAlg g' valves pop'

iterateGenAlgWithEl :: RandomGen g => g -> Valves -> ElPopulation -> ([Int], g)
iterateGenAlgWithEl g valves pop = (maximum (fst <$> scores) : nextScores, g'')
    where 
        scores = scorePopulationEl valves pop
        (pop', g') = mutatePopulation mutateIndividualEl g $ cullPopulation 100 scores
        (nextScores, g'') = iterateGenAlgWithEl g' valves pop'



data Day16 = Day16 deriving (Show, Read, Eq)
instance Day Day16 where
    partOne :: Day16 -> String -> String
    partOne _ input = unlines $ map show $ fst $ iterateGenAlg (mkStdGen 123) valves (take 10000 $ allRoutes valves)
        where valves = HM.fromList (readValve <$> lines input)
    partTwo :: Day16 -> String -> String
    partTwo _ input = unlines $ map show $ scanl addMax (0,0) $ fst $ iterateGenAlgWithEl (mkStdGen 123) valves (splitAt 5 <$> take 1000 (allRoutes valves))
        where 
            valves = HM.fromList (readValve <$> lines input)
            addMax (m, _) y = (max m y, y) 
