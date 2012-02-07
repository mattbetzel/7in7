intersperse2 :: a -> [[a]] -> [a]
intersperse2 _ [] = []
intersperse2 _ (x:[]) = x
intersperse2 sep (x:xs) = x ++ [sep] ++ intersperse2 sep xs
