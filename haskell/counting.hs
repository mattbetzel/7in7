import Data.List

count :: (Ord a) => [a] -> [(a, Int)]
count = map mapper . group . sort
  where mapper xs@(x:_) = (x, length xs)

tupleToString :: (Show a) => (a, Int) -> String
tupleToString (x, cnt) = show x ++ ":" ++ show cnt

countToStringWith :: (Ord a, Show a) => ((a, Int) -> String) -> [a] -> String
countToStringWith f = intercalate ", " . map f . count

countToString :: (Ord a, Show a) => [a] -> String
countToString = countToStringWith tupleToString
