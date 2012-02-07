import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _               -> putStrLn "error: exactly two arguments needed"
          myFunction = transpose

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

firstWords :: String -> String
firstWords s = unlines (map firstWord (lines s))
        where firstWord l = case words l
                            of [] -> ""
                               x:xs -> x

transpose :: String -> String
transpose s = unlines (concat (map transposeTuple (collectPairs (lines s))))

collectPairs :: [a] -> [(a, a)]
collectPairs []       = []
collectPairs (x:[])   = [(x, x)]
collectPairs (x:y:xs) = (x, y) : collectPairs xs

transposeTuple :: ([a], [a]) -> [[a]]
transposeTuple (x1, x2) = map combine (zip x1 x2)
        where combine (y1, y2) = y1 : y2 : []
