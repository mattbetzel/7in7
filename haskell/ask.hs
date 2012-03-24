import System.Random
import System.IO

main = do
    num <- getStdRandom $ randomR (1, 100)
    askGuess num 1 "What is the number? "

askGuess :: Int -> Int -> String -> IO ()
askGuess num try pmt = do
    guess <- ask pmt
    case guess `compare` num of
        LT -> askGuess num (succ try) "Too low, what is it? "
        GT -> askGuess num (succ try) "Too high, what is it? "
        EQ -> putStrLn $ "Correct in " ++ show try ++ " tries!"

ask :: Read a => String -> IO a
ask str = do
    putStr str
    hFlush stdout
    res <- getLine
    return $ read res
