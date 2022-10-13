{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Ratio
import Control.Applicative

-- Let’s say that every item in the list comes with another value, a probability of it
-- happening. It might make sense to present this like this then

-- [(3,0.5),(5,0.25),(9,0.25)]
-- Floating point numbers can get real messy real fast because they tend to lose precision,
-- so Haskell offers us a data type for rational numbers that doesn’t lose precision.

-- some examples of rationals 
-- ghci> 1%4
-- ghci> 1%2 + 1%2
-- ghci> 1%3 + 5%4

-- So let's throw out our floating points and use Rational for our probabilities 
-- ghci> [(3, 1%2), (5, 1%4), (9, 1%4)]

-- We took lists and we added some extra context to them, so this represents values with contexts too.

newtype Prob a = Prob {  getProb :: [(a,Rational)] } deriving (Show) 

-- Let;s make it an instance of Functor as we would want to map functions over the elements while leaving the probabilities as they are 
instance Functor Prob where 
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

-- We unwrap it from the newtype with pattern matching, apply the function f to the values while keeping the probabilities as they are and then wrap it back up.
-- ghci> fmap negate (Prob [(3,1%2), (5,1%4), (9, 1%4)])

-- A question might be, is this a monad?
-- Well, return x is supposed
-- to make a monadic value that always presents x as its result, so it doesn’t make
-- sense for the probability to be 0. If it always has to present it as its result, the
-- probability should be 1!

-- Here is a situation where (a or b) has a 1%4 prob of happpening with a and b both being equally likely and (c or d) has 3%4 prob of happening both
-- also being equally likely 
thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [( Prob [('a',1%2), ('b', 1%2)], 1%4 )
    ,( Prob [('c',1%2), ('d', 1%2)], 3%4 )
     ]

-- So now that we’ve figured out how
-- to flatten a nested probability list, all we have to do is write the code for this
-- and then we can write >>= simply as join (fmap f m) and we have ourselves a
-- monad!
-- Here is our flatten 
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where 
        multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

-- The function multAll takes a tuple of probability list and a probability p that
-- comes with it and then multiplies every inner probability with p, returning a
-- list of pairs of items and probabilities. We map multAll over each pair in our
-- nested probability list and then we just flatten the resulting nested list.

-- We make it an Applicative instance first 
instance Applicative Prob where 
    pure x = Prob [(x,1%1)]
    -- Prob xs <*> Prob xs = 

-- We can now write the Monad instance 
instance Monad Prob where 
    return x = Prob [(x,1%1)]
    m >>= f  = flatten (fmap f m)
    -- fail _   = Prob []

-- We can check if this is indeed a Monad by checking the Monad laws 

-- The first one says that return x >>= f should be equal to f x.
-- We can see that if we put a value in
-- a default context with return and then fmap a function over that and flatten
-- the resulting probability list, every probability that results from the function
-- would be multiplied by the 1%1 probability that we made with return, so it
-- wouldn’t affect the context.
-- The reasoning for m >>= return being equal to just m is similar.

-- The third law states that f <=< (g <=< h) should be the same as
-- (f <=< g) <=< h. This one holds as well, because it holds for the list monad
-- which forms the basis of the probability monad and because multiplication is
-- associative. 1%2 * (1%3 * 1%5) is equal to (1%2 * 1%3) * 1%5

-- Now that we have this Monad we can do calculations that involve probabilities 
-- We can treat probabilistic events as values with
-- contexts and the probability monad will make sure that those probabilities get reflected in the probabilities of the final result.

-- Let's play with some coins
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin 
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedcoin :: Prob Coin 
loadedcoin = Prob [(Heads, 1%10), (Tails, 9%10)]

-- now the coin throwing action 
flipThree :: Prob Bool 
flipThree = do
    a <- coin 
    b <- coin 
    c <- loadedcoin 
    return (all (==Tails) [a,b,c])

