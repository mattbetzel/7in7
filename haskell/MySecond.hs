mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "too short!"
              else head (tail xs)
