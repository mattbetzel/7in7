import Data.Char (digitToInt, isDigit)
import Data.List (isInfixOf, tails)

asInt_fold :: String -> Int
asInt_fold s | null s || s == '-' : [] = error "not a number"
asInt_fold ('-':s) = negate (asInt_fold s)
asInt_fold s = foldl step 0 s
      where step _   char | not (isDigit char) 
                          = error (char : " isn't a digit")
            step acc char = let acc' = acc * 10 + digitToInt char
                            in if acc' > acc then acc' else error "overflow"

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either s | null s || s == '-' : [] = Left "not a number"
asInt_either ('-':s) = case asInt_either s
                       of   (Right acc) -> Right (negate acc)
                            other       -> other
asInt_either s = foldl step (Right 0) s
      where step _           char | not (isDigit char) 
                                  = Left (char : " isn't a digit")
            step (Right acc) char = let acc' = acc * 10 + digitToInt char
                                    in if acc' > acc then Right acc' else Left "overflow"
            step other       _    = other


myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _    []   = []
myGroupBy func (x:xs) = let (_, group, groups) = foldl step (x, [x], []) xs
                      in  groups ++ [group]
      where step (e, grp, grps) y | e `func` y = (e, grp ++ [y], grps)
                                  | otherwise  = (y, [y], grps ++ [grp])

isInAny1 needle haystack = any (needle `isInfixOf`) haystack 
isInAny2 needle = any (needle `isInfixOf`)

suffixes5 = init . tails
