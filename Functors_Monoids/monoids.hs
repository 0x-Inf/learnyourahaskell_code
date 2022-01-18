import Data.Monoid
import qualified Data.Foldable as F
-- A monoid is when you have an associative binary fn and a value that acts as identity with respect to that function (Just like the ones in C.T)
-- There are lots of monoids in the world of haskell 
-- That's why we have the Monoid type class, 
-- it's defined as:
class Monoid' m where
    mempty' :: m
    mappend' :: m -> m -> m
    mconcat' :: [m] -> m
    mconcat' = foldr mappend' mempty'

-- The Monoid type class is defined in import Data.Monoid

-- Let's get familiar 

-- We see that only concrete types can be made instances of Monoid, m in the type class definition doesn't take any type parameters
-- mempty represents the identity value for a particular monoid. (it's not a fn but more like a polymorphic constant)
-- mappend takes two monoid values and returns another (its the Monoidal product in C.T lingo)
-- mconcat takes a list of monoid values and reduces them to a single value by doing mappend between the list's elements (it's kind of 'optional')

-- Some laws of monoids, we have to follow when making instances of monoids
--      mempty `mappend` x = x
--      x `mappend` mempty = x
--      (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- Haskell doesn't enforce these laws so it's upto the programmer to acertain that our instances do obey them

-- We'll look at a few examples

-- lists are monoids
instance Monoid' [a] where
    mempty' = []
    mappend' = (++)

-- Notice how we wrote [a] because we require a concrete type for an instance
-- e.g
-- ghci> [1,2,3] `mappend` [45,6,7]
-- ghci> mconcat [[12,34], [3,4], [8,9]]
-- ghci> mempty :: [a]

--  Product and sum
-- for * and + we see that there are two ways in which numbers can be monoids
-- a question can arise as to which one to use
-- We don't have to choose since when there are several ways for some type to be an instance of the same type class,
-- we can wrap that type in a newtype and then make the new type an instance of the type class in a different way

-- the Data.Monoid module exports two types for this, namely Product and Sum
-- Product is defined as:
newtype Product' a = Product' { getProduct' :: a}
    deriving (Eq, Ord, Read, Show, Bounded)

-- it's instance of monoid goes something like this 
instance Num a => Monoid' (Product' a) where
    mempty' = Product' 1
    Product' x `mappend'` Product' y = Product' (x * y)

-- ghci> getProduct $ Product 3 `mappend` Product 9
-- ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
-- ghci> getProduct . mconcat . map Product $ [3,4,2]

-- Sum is similar to Product


-- Any and All

-- Another type which can act like a monoid in two distinct but equally valid ways is Bool. 
-- The first way is to have the or function || act as the binary function along with False as the identity value.
-- The Any newtype constructor is an instance of Monoid in this fashion. It's defined like this: 
newtype Any' = Any' { getAny':: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
-- it's instance goes like:
instance Monoid' Any' where 
    mempty' = Any' False
    Any' x `mappend'` Any' y = Any' (x || y)

-- ghci> getAny $ Any True `mappend` Any False
-- ghci> getAny $ mempty `mappend` Any True
-- ghci> getAny . mconcat . map Any $ [False, False, False, True]

-- The other way for Bool to be an instance of Monoid is
-- have && be the binary fn and then make True the identity value
-- This is the newtype declaration 
newtype All' = All' {getAll' :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
-- and the instance 
instance Monoid' All' where
    mempty' = All' True
    All' x `mappend'` All' y = All' ( x && y)

-- ghci> getAll $ mempty `mappend` All True
-- ghci> getAll . mconcat . map All $ [True, True, True]


-- The Ordering monoid
-- Here is ordering as a monoid instance
{-
instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
-}

-- So how is this monoid useful 
-- say you were writing a function that takes two strings, compares their lengths, and returns an Ordering
-- But if the strings are of the same length, then instead of returning EQ right away, we want to compare them alphabetically
-- one way to do this would be:
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a

-- By employing our understanding of how Ordering is a monoid, we can rewrite the fn in a simpler manner
lengthCompare' :: String -> String -> Ordering 
lengthCompare' x y = (length x `compare` length y) `mappend`
                     (x `compare` y)

-- If we want to mod this to compare for the number of vowels 
lengthCompare'' :: String -> String -> Ordering 
lengthCompare'' x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

-- The Ordering monoid is very cool because it allows us to easily compare things by many different criteria and put those criteria in an order themselves


-- Maybe the monoid 
-- a way to treat Maybe a is a monoid only if its type parameter a is a monoid as well
-- then implement mappend in such a way that it uses the mappend operation  of the values that are wrapped with Just
-- we use Nothing as the identity 
-- Here's the instance declaration
{-
instance Monoid a =>  Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-}
-- ghci> Nothing `mappend` Just "andy"
-- ghci> Just LT`mappend` Nothing
-- ghci> Just (Sum 3) `mappend` Just (Sum 4)

-- This comes in handy when dealing with monoids as results of computations that may have failed 

-- What if the contents of Maybe are not an instance of Monoid
-- Well, one thing we can do is to just discard the second value and keep the first one
--  For this, the First a type exists and this is its definition: 
newtype First' a = First' { getFirst' :: Maybe a}
    deriving (Eq, Ord, Read, Show)
-- We take a Maybe a and wrap it with a newtype. 
-- The Monoid instance is as follows
instance Monoid' (First' a) where
    mempty' = First' Nothing
    First' (Just x) `mappend'` _ = First' (Just x)
    First' Nothing `mappend'` x = x

-- ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')
-- ghci> getFirst $ First Nothing `mappend` First (Just 'b')
-- ghci> getFirst $ First (Just 'a') `mappend` First Nothing 
-- First is useful when we have a bunch of Maybe values and we just want to know if any of them is a Just.
-- ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]

-- If we want a monoid on Maybe a such that the second parameter is kept if both params of `mappend` are Just values
    -- Data.Monoid provides a Last a type
-- ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
-- ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")



-- Using monoids to fold data structures
-- We can use folds not just on lists but on almost any data structure 
-- This is why the Foldable type class was introduced
-- Foldable is for things that can be folded up! 

-- ghci> foldr (*) 1 [1,2,3]
-- ghci> F.foldr (*) 1 [1,2,3]

-- some of the data structures that support folds
-- there's is Maybe 
-- ghci> F.foldl (+) 2 (Just 9)
-- ghci> F.foldr (||) False (Just True)
-- This kinda acts like a list with one value

-- Let's examine a data structure a little more complex
data Tree a = Empty | Node a (Tree a ) (Tree a) deriving (Show, Read, Eq)

-- We're going to make it an instance of foldable so as to get the ability to fold it up
-- just like when we made it an instance of Functor so as to map functions over it
-- We will use the foldMap fn which has type:
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- if we just implement foldMap for some type, we get foldr and foldl on that type for free
-- This is how we make Tree an instance Foldable
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

-- consider this tree
testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

-- ghci> F.foldl (+) 0 testTree
-- ghci> F.foldl (*) 1 testTree

-- if we want to know if any number in our tree is equal to 3 we can do this
-- ghci> getAny $ F.foldMap (\x -> Any $ x == 15) testTree
-- ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree

-- converting our Tree to list
-- ghci> F.foldMap (\x -> [x]) testTree

-- These tricks are not just limited to trees, they work on any instance of Foldable