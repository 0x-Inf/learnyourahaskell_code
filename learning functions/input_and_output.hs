-- Functions in haskell don't change state and this helps us reason about our programs.
-- There's a problem with that however, If a function can't change anything in the world, how is it supposed to tell us what it calculated?
-- In order to tell us what it calculated, it has to change the state of an output device (usually the state of the screen),
-- which then emits photons that travel to our brain and change the state of our mind, man.

-- All is not lost however as haskell provides a way to separate the pure functional parts of our programs from the impure ones
-- which does all the dirty work like talking to the keyboard and the screen
-- With those two parts separated, we can still reason about our pure program and take advantage of all the things that purity offers
-- ike laziness, robustness and modularity while efficiently communicating with the outside world.


-- main = do
--     putStrLn "Hello, what do i call you?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ " , you rock!")

-- Every I/O action that gets performed has a result encapsulated within it.
-- The above example program could be written as 
-- main = do
--     foo <- putStrLn "Hello, what do i call you?"
--     name <- getLine  
--     putStrLn ("Hey " ++ name ++ ", you rock!")  

--  in a do block, the last action cannot be bound to a name like the first two were.

-- Using let bindings in do blocks
-- They have to be in the form of let bindings in expression
-- where bindings are names to be given to expressions and expression is the expression that is to be evaluated that sees them.
import Data.Char

-- main = do 
--     putStrLn "What's your first name?"
--     firstName <- getLine 
--     putStrLn "What's your second name?"
--     secondName <- getLine 
--     let bigFirstName = map toUpper firstName
--         bigSecondName = map toUpper secondName
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigSecondName ++ ", how are you?"


-- Now we're going to make a program that continuously reads a line and prints out the same line with the words reversed
-- The program's execution will stop when we input a blank line.
main = do
    line <- getLine
    if null line    
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- the return statement above is different from the return in imperative languages.
-- In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value
-- The resulting I/O action doesn't actually do anything, it just has that value encapsulated as its result.

-- more exploration of return 
main' = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line  

-- All these returns do is that they make I/O actions that don't really do anything except have an encapsulated result and that result is thrown away because it isn't bound to a name.
-- We can use return in combination with <- to bind stuff to names.
main'' = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

-- some fn useful when dealing with I/O
-- putStr is much like putStrLn in that it takes a string as a parameter and returns an I/O action that will print that string to 
-- terminal, only putStr doesn't jump into a new line after printing out the string while putStrLn does.

-- putChar takes a character and returns an I/O action that will print it out to the terminal.
-- print takes a value of any type that's an instance of Show  calls show with that value to stringify it and then outputs that string to the terminal. 

-- getChar is an I/O action that reads a character from the input
main''' = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()


-- sequence takes a list of I/O actions and returns an I/O actions that will perform those actions one after the other. 
-- The result contained in that I/O action will be a list of the results of all the I/O actions that were performed
-- doing this
-- main = do
--     a <- getLine
--     b <- getLine 
--     c <- getLine
--     print [a,b,c]

-- -- is exactly same as 
-- main = do
--     rs <- sequence [getLine, getLine, getLine]
--     print rs
        
-- Because mapping a function that returns an I/O action over a list and then sequencing it is so common, the utility functions mapM and mapM_ were introduced.
-- mapM takes a function and a list, maps the function over the list and then sequences it. mapM_ does the same, only it throws away the result later.




