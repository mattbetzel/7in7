data Direction = Lefter 
               | Righter 
               | Straighter
                 deriving (Show)

direction :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Direction
direction a b c = case compare (crossprod a b c) 0 of
        GT -> Lefter
        LT -> Righter
        EQ -> Straighter
    where crossprod (x1, y1) (x2, y2) (x3, y3) = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

directions :: (Num a, Ord a) => [(a, a)] -> [Direction]
directions (a:b:c:xs) = (direction a b c) : (directions (b : c : xs))
directions _ = []
