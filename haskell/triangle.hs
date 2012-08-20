triangles :: Int -> Int -> [(Int, Int, Int)]
triangles n p = [ (a, b, c) | a <- uptoN, b <- uptoN, c <- uptoN, right a b c, perimeter a b c == p ]
  where uptoN = [1..n]
        right a b c = a * a + b * b == c * c
        perimeter a b c = a + b + c

applytwice :: (a -> a) -> a -> a
applytwice f = f . f

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

ander :: (a -> Bool) -> (a -> Bool) -> a -> Bool
ander f1 f2 x = f1 x && f2 x

modpred n = (==0) . (`mod`n) 
