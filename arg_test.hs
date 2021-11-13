import System.Environment
import System.IO
import Data.List

main = do
    args <- getArgs 
    progName <- getProgName 
    putStrLn "The arguments are: "
    mapM_ putStrLn args
    putStrLn "The program name is: "
    putStrLn progName


