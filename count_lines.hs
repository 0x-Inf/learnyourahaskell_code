import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (lenght (lines contents)) ++ " lines!"

-- simple handler
{-
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!" 
-}

-- we use pipes to know which error occurred and do smth if it's the error we're interested in, otherwise rethrow it

handler :: IOError -> IO ()
handler e 
    | isDoesNotExistError e = putStrLn "The file does not exist!"
    | otherwise = ioError e

-- System.IO.Error also exports functions that enable us to ask our exceptions for some attributes, like what the handle of the file that caused the error is, or what the filename is
-- like this one which returns the filePath (a.k.a String)
hnadler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just Path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError e
