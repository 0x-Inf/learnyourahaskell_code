-- import Control.Applicative
import qualified Control.Applicative as A
-- import Main (Applicative)

-- We'll be exploring the Applicative typeclass
-- It defines two methods pure and <*>
-- The class is defined as follows 
{-
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}
-- the definition above tells us a lot 
-- We have a type constraint which tells if we want to make a type constructor part of the Applicative typeclass, it has to be in Functor first. 

-- the 1st method it defines is called pure
-- Its type declaration is pure :: a -> f a. f plays the role of our applicative functor instance here.
-- We take a value and we wrap it in an applicative functor that has that value as the result inside it.

-- the <*> is interesting 
--  It has a type declaration of f (a -> b) -> f a -> f b which is similar to fmap :: (a -> b) -> f a -> f b
-- It's sort of a beefed up fmap
-- <*> takes a functor that has a function in it and another functor and sort of extracts that function from the first functor and then maps it 
-- over the second one
data Maybe' a = Nothing' | Just' a deriving (Show)

data PlayerToken a = PlayerToken {
    name :: String,
    clan :: String,
    hp :: Integer
} deriving (Show , Ord , Eq)

instance Functor Maybe' where
    fmap f (Just' x) = Just' (f x)
    fmap f Nothing' = Nothing'

-- reverse' :: String -> String
-- reverse' = SortBy Down 

-- let's look at the Applicative instance implementation of Maybe
instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    (Just' f) <*> something = fmap f something

-- Applicative functors, on the other hand, allow you to operate on several functors with a single function. Check out this piece of code:
-- ghci> pure (+) <*> Just 3 <*> Just 5
-- ghci> pure (+) <*> Just 3 <*> Nothing
-- ghci> pure (*) <*> Nothing <*> Just 8


-- This is why Control.Applicative exports a function called <$>, which is just fmap as an infix operator. (question may come later)
-- Here's how it's defined 
{-
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
-}

-- By using <$>, the applicative style really shines, because now if we want to apply a function f between three applicative functors
-- We can apply this as f <$> x <*> y <*> z
-- If the parameters weren't applicative functors  but normal values we'll write 
-- f x y z

-- example for joining strings
-- ghci> (++) <$> Just "johntra" <*> Just "volta"
-- ghci> (++) "johntra" "volta"
-- to use a normal function on applicative functors, just splinkle some <$> and <*> about and the function will operate on applicatives and 
-- return an applicative (this is cool)

-- Lists (actually the list type constructor ,[] )  are applicative functors 
-- Here's how [] is an instance  of Applicative 
{-
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <-- fs, x <- xs ]
-}
-- Some examples 
-- ghci> pure "Hey" :: [String]

-- We apply every possible function from the left list to every possible value from the right list
-- ghci> [(*0),(+100),(^2)] <*> [1,2,3]
-- ghci> [(*), (+)] <*> [1,2] <*> [3,5]
-- ghci> (++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "," "."]

-- ghci> (*) <$> [2,5,10] <*> [8,10,11]
-- ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]

-- Another instance of Applicative that we've already encountered is IO. 
-- This is how the instance is implemented:
{-
instance Applicative IO where 
    pure = return 
    a <*> b = do 
        f <- a 
        x <- b 
        return (f x)
-}

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

-- Another way of doing this would be to use the applicative style
myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

-- The type of the expression (++) <$> getLine <*> getLine is IO String
-- Hence we can do:
main = do 
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a


-- Another instance of Applicative is (->) r, so functions. 
-- here is how it's implemented 
{-
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
-}

-- examples
-- ghci> pure 3 "balh"
-- ghci> :t (+) <$> (+3) <*> (*100)
-- ghci> (+) <$> (+3) <*> (*100) $ 5
-- ghci> (\x y z -> [x, y, z]) <$> (+3) <*> (*2) <*> (/2) $ 5

--  the ZipList a type was introduced, which has one constructor ZipList that has just one field, and that field is a list.
{-
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> fx) fs)
-}
-- examples
-- ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100, 100, 100]
-- ghci> getZipList $ (+) <$> ZipList [1,2,4] <*> ZipList [100, 100..]
-- ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,4,1,2] 
-- ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "tap"

-- use the applicative style to zip together an arbitrary amount of lists with a function

-- Control.Applicative defines a function that's called liftA2
{-
liftA2 :: (Applicative f) => (a, b, c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
-}
-- Nothing special, it just applies a function between two applicatives
-- with applicative functors, we can apply a function between several functors. 
-- we can take two applicative functors and combine them into 
-- one applicative functor that has inside it the results of those two applicative functors in a list
-- example 
-- we have Just 3 and Just 4. Let's assume that the second one has a singleton list inside it
-- ghci> fmap (\x -> [x]) (Just 4)
-- Here's how we get Just [3,4]
-- ghci> liftA2 (:) (Just 3) (Just [4])
-- ghci> (:) <$> Just 3 <*> Just [4]
-- we could just keep combining applicative functors and storing the results in another applicative functor as a list

-- a function that takes a list of applicatives and returns an applicative that has a list as its result value
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (A.liftA2 (:)) (pure [])

-- examples 
-- ghci> sequenceA [Just 3, Just 2, Just 1]
-- ghci> sequenceA [Just 3, Nothing, Just 6]
-- ghci> sequenceA [(+3), (*4), (+1)] 4
-- ghci> sequenceA [[1,2,3], [4,5,6]]

-- Using sequenceA is cool when we have a list of functions and we want to feed the same input to all of them and then view the list of results.
-- e.g checking if a number satisfies certain predicates
-- ghci> map (\f -> f 7) [(>4), (<10), odd]
-- ghci> and $ map (\f -> f 7) [(>4), (<10), odd]
-- we can achieve the above by using sequenceA
-- ghci> sequenceA [(>4), (<10), odd] 6
-- ghci> and $ sequence [(>4), (<10), even]

-- exploring sequenceA with [] 
-- ghci> sequenceA [[1,2,3], [4,5,6]]
-- ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]

-- doing (+) <$> [1,2] <*> [4,5,6]results in a non-deterministic computation x + y where x takes on every value from [1,2] and y takes on every value from [4,5,6]
-- we store the result of a non-deterministic computation in, we use a list, whereeach element in the list is one possible list.

-- When used with I/O actions, sequenceA is the same thing as sequence! 
-- It takes a list of I/O actions and returns an I/O action that will perform each of those actions and have as its result a list of the results of those I/O actions
-- ghci> sequenceA [getLine, getLine, getLine]
-- Ps: you can't get the result  of an I/O action without performing it.

-- Just like normal functors, applicative functors also have laws, 
-- The most important one being: pure f <*> x = fmap f x
-- The other ones are:
--      pure id <*> v = v
--      pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--      pure f <*> pure x = pure (f x)
--      u <*> pure y = pure ($ y) <*> u

-- applicative functors are useful as they allow us to combine different computations
-- such as I/O computations, non-deterministic computations, computations that might have failed, etc. 
