import Control.Monad.Writer
import Data.List

-- Consider the filter fn that's defined as follows
{-filter :: (a -> Bool) -> [a] -> [a] -}

-- So if the Bool that the predicate returned came with a context, we’d
-- expect the final resulting list to have some context attached as well, otherwise
-- the context that each Bool came with would be lost.

-- The filterM fn from Control.Monad does just what we want
{-filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a] -}

-- The predicate returns a monadic value whose result is a Bool, but because
-- it’s a monadic value, its context can be anything from a possible failure to
-- non-determinism and more! To ensure that the context is reflected in the final
-- result, the result is also a monadic value.

-- Let's look at the following 
-- ghci> filter (\x -> x < 4) [9,1,5,2,10,3]

-- Now, let’s make a predicate that, aside from presenting a True or False result, also provides a log of what it did.

keepSmall :: Int -> Writer [String] Bool 
keepSmall x
    | x < 4 = do
        tell ["keeping " ++ show x]
        return True 
    | otherwise = do 
        tell [show x ++ " is too large, throwing it away"]
        return False 

-- Instead of just returning a Bool, this fn returns a Writer [String] Bool. It's a monadic predicate 
-- Let's give it to filterM along with a list 
-- ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- We can print the log and see what we got 
-- ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

-- So just by providing a monadic predicate to filterM, we were able to filter a list while taking advantage of the monadic context that we used.
-- A very cool Haskell trick is using filterM to get the powerset of a list (If we think of them as sets for now)

-- To make a function that returns a powerset of some list, we’re going to rely on non-determinism.
-- So we are going to filter a list and we’ll use a predicate that non-deterministically both keeps and drops every element from the list.

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- We choose to drop and keep every element, regardless of
-- what that element is. We have a non-deterministic predicate, so the resulting
-- list will also be a non-deterministic value and will thus be a list of lists.

-- ghci> powerset [1,2,3]

-- This takes a bit of thinking to wrap your head around, but if you just consider
-- lists as non-deterministic values that don’t know what to be so they just decide
-- to be everything at once, it’s a bit easier.


-- foldM 

-- The monadic counterpart for foldl is foldM. Just like foldl foldM takes a binary fn that produces a monadic value and folds the list up with that 
-- the resulting value is also monadic 
-- the type of foldl is this: 
{-
foldl :: (a -> b -> a) -> a -> [b] -> a
-- whereas foldM is 
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
-}
-- The value that the binary fn returns is Monadic and so the result of the whole fold is as well 

-- ghci> foldl (\acc x -> acc + x) 0 [2,8,7,8]

-- Now what if we wanted to sum a list of numbers but with the added condition that if any number is greater than 9 in the list, the whole thing fails?

binSmalls :: Int -> Int -> Maybe Int 
binSmalls acc x 
    | x > 9     = Nothing 
    | otherwise = Just (acc + x)

-- Because our binary fn is monadic, we can't use it with the normal foldl but with foldM
-- ghci> foldM binSmalls 0 [7,4,5,6,3]
-- ghci> foldM binSmalls 0 [7,4,5,6,11]

-- Folding with a binary function that returns a Writer value is cool as well because then you log whatever you want as your fold goes along its way.

-- Making the RPN calculator safe 
-- let’s take our RPN calculator and add error handling to it by taking advantage of the Maybe monad.

-- The implementation was something like this 
-- It takes a list and then folding over that list by starting out with an empty stack and then using a
-- binary folding function that adds numbers to the stack or manipulates numbers
-- on the top of the stack to add them together and divide them and such.

solveRPN :: String -> Double 
solveRPN = head . foldl foldingFunction [] . words 
    where 
        foldingFunction :: [Double] -> String -> [Double]
        foldingFunction (x:y:ys) "*" = (x * y):ys
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (x - y):ys
        foldingFunction xs numberString = read numberString:xs

-- Let's make our folding fn capable of graceful failure
-- This is it's new type 
-- foldingFunction' :: [Double] -> String -> Maybe [Double]

-- Here is a modified fn that behaves like read but it has to consume the full input to work 
readMaybe :: (Read a) => String -> Maybe a 
readMaybe st = case reads st of [(x,"")] -> Just x
                                _        -> Nothing 

-- ghci> readMaybe "1" :: Maybe Int 
-- ghci> readMaybe "GO SOME PLACe" :: Maybe Int

-- Let's make our foldingFunction into a monadic one that can fail 
foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:ys) "*" = return ((x * y):ys)
foldingFunction' (x:y:ys) "+" = return ((x + y):ys)
foldingFunction' (x:y:ys) "-" = return ((x - y):ys)
foldingFunction' xs numberString = liftM (:xs) (readMaybe numberString)

-- Putting it to the test 
-- ghci> foldingFunction' [3,2] "*"
-- ghci> foldingFunction' [3,2] "-"
-- ghci> foldingFunction' [] "*"
-- ghci> foldingFunction' [] "1"
-- ghci> foldingFunction' [] "1 wawawawa"

-- So now the new and improved RPN 
solveRPN' :: String -> Maybe Double 
solveRPN' st = do
    [result] <- foldM foldingFunction' [] (words st)
    return result 

-- Walk through the semantics of the above and see where it returns Nothing 
-- some examples 
-- ghci> solveRPN' "1 2 * 4 +"
-- ghci> solveRPN' "1 2 * 4 + 5 *"
-- ghci> solveRPN' "1 2 * 4"
-- ghci> solveRPN' "1 2 wharglbllargh"

-- The first failure happens because the final stack isn’t a list with one element in
-- it and so the pattern matching in the do expression fails. The second failure happens because readMaybe returns a Nothing.

-- Composing monadic functions 
-- If we have a bunch of functions in a list, we can compose them one all into one big function by just using id as the starting accumulator and the . function as the binary function.
-- ghci> let f = foldr (.) id [(+1), (*100), (+1)]
-- ghci> f 1

-- We can compose monadic fns the same way 
-- only instead of normal composition we use <=< and instead of id
-- we use return. We don’t have to use a foldM over a foldr or anything because
-- the <=< function makes sure that composition happens in a monadic fashion.

-- Using the knights quest example we can make the in3 fn to take any arbitrary move number 
-- to generate all the possible positions that he can have after
-- taking three moves, we made the following function:

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- and to check if he can go from start to end in 3 moves, we did:
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start  

-- Let's make in3 more general 
inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

-- First we use replicate to make a list that contains x copies of the function
-- moveKnight. Then, we monadically compose all those functions into one, which
-- gives us a function that takes a starting position and non-deterministically moves
-- the knight x times. Then, we just make the starting position into a singleton
-- list with return and feed it to the function.

-- now we can change the canReachIn3 to be more general as well
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start 
