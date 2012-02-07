mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

-- mymean :: [Num] -> Fractional
mymean [] = 0
mymean xs = mysum xs / fromIntegral (mylength xs)
          where mysum [] = 0
                mysum (x:xs) = x + mysum xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

palandrome :: [a] -> [a]
palandrome xs = xs ++ rev xs

-- isPalandrome :: [a] -> Bool
isPalandrome xs = xs == rev xs
