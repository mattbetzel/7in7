import Data.Char (isSpace, isDigit)
import Data.List (groupBy)

input = "10 4 3 + 2 * -"
input' = "  10  4  3  +  2 * -"
input'' = "  10.3  4.2  3.0  +  2 * -"

rpn :: (Num a, Read a) => String -> a
rpn = head . foldl stacker [] . words
    where stacker s t = case parseToken t of
            (Just (Left num)) -> num : s
            (Just (Right op)) -> performOp op s

type Op a = a -> a -> a

performOp :: Op a -> [a] -> [a]
performOp op (x:y:xs) = op y x : xs

parseToken :: (Read a, Num a) => String -> Maybe (Either a (Op a))
parseToken str = case getNum str of
        Just num -> Just $ Left num
        Nothing  -> case parseOp str of
            Just op -> Just $ Right op
            Nothing -> Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead x = case reads x of   
        ((x', ""):[]) -> Just x'
        _              -> Nothing

getNum :: (Read a, Num a) => String -> Maybe a
getNum s = maybeRead s

parseOp :: Num a => String -> Maybe (Op a)
parseOp str = lookup str opTable

opTable :: Num a => [(String, Op a)]
opTable = [("+", (+)), ("-", (-)), ("*", (*))] --, ("/", (/))]
