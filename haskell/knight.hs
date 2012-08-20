import Control.Monad
import Data.List (find,tails)
import GHC.Exts (sortWith)

type Position = (Int, Int)
type Positions = [Position]

newPositions :: Positions -> [Positions]
newPositions xs@((c, r) : _) = do
    a <- [-1, -2, 1, 2]
    b <- [-1, -2, 1, 2]
    guard (abs a /= abs b)
    let pos@(c', r') = (c+a, r+b)
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (pos : xs)

positions3 :: Positions -> [Positions]
positions3 p = manyTimes 3 (return p) newPositions

path3 :: Position -> Position -> Maybe Positions
path3 s e = let pos = paths3 s in fmap reverse $ find (\x -> head x == e) pos

paths3 :: Position -> [Positions] 
paths3 = sortWith length . concat . map init . map tails . positions3 . return 

manyTimes :: Monad m => Int -> m a -> (a -> m a) -> m a
manyTimes 0 s f = s
manyTimes n s f = manyTimes (n-1) (s >>= f) f

manyTimes :: Monad m => Int -> (a -> m a) -> (a -> m a)
manyTimes n 
