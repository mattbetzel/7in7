import Data.Char (isSpace, isDigit)
import Data.List (groupBy)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

input = "10 4 3 + 2 * -"
inputSpace = "  10  4  3  +  2 * -"
inputDouble = "  10.3  4.2  3.0  +  2 * -"
inputBad = "sdf"
inputUnbalanced = "4 3 2 +"
inputBadOp = "4 +"

main = do
    (x:_) <- getArgs
    maybe (putStrLn "Invalid input.") print (rpn x)

rpn :: (Num a, Read a) => String -> Maybe a
rpn str = computeStack str >>= justHead

justHead :: [a] -> Maybe a
justHead (x:[]) = Just x
justHead _      = Nothing

computeStack :: (Num a, Read a) => String -> Maybe [a]
computeStack = foldl stacker (Just []) . words

stacker :: (Num a, Read a) => Maybe [a] -> String -> Maybe [a]
stacker m t = m >>= (\s -> case parseToken t of
        (Just (Left num)) -> Just $ num : s
        (Just (Right op)) -> performOp op s
        Nothing           -> Nothing)

type Op a = a -> a -> a

performOp :: Op a -> [a] -> Maybe [a]
performOp op (x:y:xs) = Just $ op y x : xs
performOp _  _        = Nothing

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
