import Data.List (sortBy)

sortLists :: [[a]] -> [[a]]
sortLists xs = sortBy listCmp xs
    where listCmp a b = compare (length a) (length b)
