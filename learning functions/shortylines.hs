-- main = do
--     contents <- getContents 
--     putStr (shortLinesOnly contents)




-- We can modify our code to be shorter by making use of the interact fn
-- interact takes a function of type String -> String as a parameter and returns an I/O action that will take some input, run that function on it and then print out the function's result
-- main = interact shortLinesOnly


-- shortLinesOnly :: String -> String 
-- shortLinesOnly input =
--     let allLines = lines input
--         shortLines = filter (\line -> length line < 10) allLines
--         result = unlines shortLines
--     in result


-- The above can even be shortned further by making use of fn composition 
main  = interact $ unlines . filter ((<10) . length) . lines

