import System.Random

randomStream :: (RandomGen g, Random a) => g -> [a]
randomStream g = 
    let (n, g') = random g
    in  n : randomStream g' 
