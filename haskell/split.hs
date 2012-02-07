splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = case span p xs
                 of ([], x:b) -> splitWith p b
                    (f, x:b)  -> f : splitWith p b
                    _         -> []
