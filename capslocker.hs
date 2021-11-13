import Data.Char

-- main = forever $ do
--     putStrLn "Give me some input: "
--     l <- getLine 
--     putStrLn $ map toUpper l


-- Instead of using forever and getLine as above we could use getContent instead
main = do 
    contents <- getContents 
    putStr (map toUpper contents)