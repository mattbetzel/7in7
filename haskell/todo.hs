import System.IO     
import System.Environment
import System.Directory
import Data.List (delete)
    
main = do
    args <- getArgs 
    runCmd args

runCmd :: [String] -> IO ()
runCmd ("view":path:[])       = viewCmd path
runCmd ("add":path:todo:[])   = addCmd path todo
runCmd ("delete":path:idx:[]) = deleteCmd path $ read idx
runCmd ("bump":path:idx:[]) = bumpCmd path $ read idx

viewCmd :: FilePath -> IO ()
viewCmd path = withFile path ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

addCmd :: FilePath -> String -> IO ()
addCmd path todo = appendFile path $ todo ++ "\n"

deleteCmd :: FilePath -> Int -> IO ()
deleteCmd path idx = hReplaceLines path $ (`deleteIndex` idx)

deleteIndex :: Eq a => [a] -> Int -> [a]
deleteIndex xs idx = delete (xs !! idx) xs

bumpIndex :: Eq a => [a] -> Int -> [a]
bumpIndex xs idx = let x = xs !! idx
                   in  x : delete x xs

bumpCmd :: FilePath -> Int -> IO ()
bumpCmd path idx = hReplaceLines path $ (`bumpIndex` idx)

withLines :: ([String] -> [String]) -> String -> String
withLines f = unlines . f . lines

hReplaceLines :: FilePath -> ([String] -> [String]) -> IO ()
hReplaceLines path f = hReplaceContents path $ withLines f

hReplaceContents :: FilePath -> (String -> String) -> IO ()
hReplaceContents rPath f = do
    rHandle <- openFile rPath ReadMode
    (wPath, wHandle) <- openTempFile "." "temp"
    contents <- hGetContents rHandle
    hPutStr wHandle $ f contents
    hClose wHandle
    hClose rHandle
    removeFile rPath
    renameFile wPath rPath
