{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}



import Control.Monad.Writer 
import Control.Monad
import Data.Monoid
-- Euclid’s algorithm is an algorithm that takes two numbers and computes their greatest common divisor

gcd' :: Int -> Int -> Int
gcd' a b 
    | b == 0    = a
    | otherwise = gcd' b (a `mod` b)

-- Now, we want to equip our result with a context, and the
-- context will be a monoid value that acts as a log. Like before, we’ll use a list of
-- strings as our monoid. So the type of our new gcd' function should be:

gcd'' :: Int -> Int -> Writer [String] Int 
gcd'' a b
    | b == 0   = do
        tell ["Finsihed with  " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)

-- We can also get the individual values in the newtype Writer since it unwraps into tuple 
-- ghci> fst $ runWriter (gcd'' 8 3)
-- Because the log is a list of strings, let’s use mapM_ putStrLn to print those strings to the screen:
-- ghci> mapM_ putStrLn $ snd $ runWriter (gcd'' 8 3)

-- It's awesome that we can make the machine report what's it's doing (at least to the level we pre-knew)
-- by changing normal values to monadic values and letting the implementation of >>= for Writer take care of the logs for us.

-- Inefficient List construction 
-- When using the Writer monad, you have to be careful which monoid to use,
-- because using lists can sometimes turn out to be very slow. That’s because lists
-- use ++ for mappend and using ++ to add something to the end of a list is slow if
-- that list is really long.

-- Think about how it happens in gcd''

-- This is an inefficient version of gcd''
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b 
    | b == 0  = do 
        tell ["Finished with "++ show a]
        return a 
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result 


-- Difference Lists 
-- Because lists can sometimes be inefficient when repeatedly appended in this
-- manner, it’s best to use a data structure that always supports efficient appending.
-- One such data structure is the difference list

-- A difference list is similar to a list,only instead of being a normal list, it’s a function that takes a list and prepends another list to it.
-- The difference list equivalent of a list like [1,2,3] would be the function \xs -> [1,2,3] ++ xs.
-- A normal empty list is [], whereas an empty difference list is the function \xs -> [] ++ xs.

-- appending two difference lists can be done as follows 
-- f `apppend` g = \xs -> f (g xs)
-- This makes a funcion that is equivalent to 
-- \xs -> "dog" ++ ("meat" ++ xs)

-- Let’s make a newtype wrapper for difference lists so that we can easily give them monoid instances:
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a 
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- We make it an instance of Semigroup 
instance Semigroup (DiffList a) where 
    (DiffList a) <> (DiffList b) = DiffList (\xs -> a (b xs))

-- Here's the Monoid instance 
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- Notice how for lists, mempty is just the id function and mappend is actually just function composition.
-- ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])

-- Now we can increase the efficiency of our gcdReverse function by making it use difference lists instead of normal lists:
gcd''' :: Int -> Int -> Writer (DiffList String) Int
gcd''' a b 
    | b == 0 = do 
        tell (toDiffList ["Finished with " ++ show a])
        return a 
    | otherwise = do
        result <- gcd''' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result 

-- We only had to change the type of the monoid from [String] to DiffList
-- String and then when using tell, convert our normal lists into difference lists
-- with toDiffList. Let’s see if the log gets assembled properly:
-- ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34

-- Comparing Performance

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

-- If we give it 0, it just logs it. For any other number, it first counts down its predecessor to 0 and then appends that number to the log.
-- Example 
-- ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000

-- However when we use normal lists instead of difference lists
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do 
    tell ["0"]
finalCountDown' x = do 
    finalCountDown' (x-1)
    tell [show x]

-- The counting is so slow with the second one 
