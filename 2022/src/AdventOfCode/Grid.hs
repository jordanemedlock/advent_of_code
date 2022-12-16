module AdventOfCode.Grid where


import qualified Data.Vector as V
import Data.Vector ((!), (//))

data Grid a = Grid 
    { gridVec :: V.Vector (V.Vector a) -- Row major 2d vector
    , gridSize :: (Int,Int) 
    }

instance Show a => Show (Grid a) where
    show :: Show a => Grid a -> String
    show (Grid vec _) = unlines $ drawRow <$> V.toList vec
        where drawRow row = concatMap show $ V.toList row

initGrid :: (Int, Int) -> a -> Grid a
initGrid (w, h) i = Grid (V.replicate h $ V.replicate w i) (w, h)

indexGrid :: Grid a -> (Int, Int) -> a
indexGrid (Grid grid _) (x,y) = grid ! y ! x

setGrid :: Grid a -> (Int, Int) -> a -> Grid a
setGrid grid pos x = updateGrid grid pos (const x)

updateGrid :: Grid a -> (Int, Int) -> (a -> a) -> Grid a
updateGrid (Grid vec size) (x, y) f = Grid (vec // [(y, vec ! y // [(x, f (vec ! y ! x))])]) size
