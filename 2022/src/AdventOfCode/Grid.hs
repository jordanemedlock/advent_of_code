module AdventOfCode.Grid where


import qualified Data.Vector as V
import Data.Vector ((!), (//))

data Pair = Pair { pX :: Int, pY :: Int } deriving (Eq)
type Point = Pair
type Size = Pair
instance Show Pair where
    show :: Pair -> String
    show (Pair x y) = show (x,y)

data Grid a = Grid 
    { gridVec :: V.Vector (V.Vector a) -- Row major 2d vector
    , gridSize :: Size
    }

instance Show a => Show (Grid a) where
    show :: Show a => Grid a -> String
    show (Grid vec _) = unlines $ drawRow <$> V.toList vec
        where drawRow row = concatMap show $ V.toList row

initGrid :: Size -> a -> Grid a
initGrid size@(Pair w h) i = Grid (V.replicate h $ V.replicate w i) size

indexGrid :: Grid a -> Point -> a
indexGrid (Grid grid _) (Pair x y) = grid ! y ! x

setGrid :: Grid a -> Point -> a -> Grid a
setGrid grid pos x = updateGrid grid pos (const x)

updateGrid :: Grid a -> Point -> (a -> a) -> Grid a
updateGrid (Grid vec size) (Pair x y) f = Grid (vec // [(y, vec ! y // [(x, f (vec ! y ! x))])]) size
