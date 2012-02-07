lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne (x:[]) = error "only one element"
lastButOne xs = if length xs <= 2 then head xs else lastButOne (tail xs)
