import Control.Monad

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
    | abs (l' - r) <= 3 = Just (l', r)
    | otherwise = Nothing 
    where l' = l + n
  
landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
    | abs (r' - l) <= 3 = Just (l, r')
    | otherwise = Nothing 
    where r' = r + n

mytest :: Maybe String
mytest = do
  x <- return "hi"
  guard True
  return "he"
