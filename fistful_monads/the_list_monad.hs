-- Here we're going to look at how we use the monadic aspects of lists to bring non-determinism into our code in a clear and readable manner.

-- Using lists as applicative functors showcases this non-determinism nicely:
-- ghci> (*) <$> [1,2,3] <*> [10,100,1000]

-- This context of non-determinism translates to Monads very nicely
-- Here what the Monad instance for lists lools like 
{-
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
-}
-- Let's see how >>= works by first gaining some intuition by playing with it 
-- ghci> [3,4,5] >>= \x -> [x,-x] 
-- Let's play around with lists that fail:
-- ghci> [] >>= \x -> ["bad","mad","rad"]

-- Just like with Maybe values, we can chain several lists with >>=, propagating the non-determinism:
-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

-- Here is the above written in do notation 
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n, ch)

-- Just like with Maybe, we’re extracting the
-- elements from the monadic values and treating them like normal values and >>=
-- takes care of the context for us. The context in this case is non-determinism.

-- Using lists with do notation is very similar to list comprehensions
-- In fact, list comprehensions are just syntactic sugar for using lists as monads. In
-- the end, list comprehensions and lists in do notation translate to using >>= to
-- do computations that feature non-determinism.

-- List comprehensions allow us to filter our output.
-- ghci> [x | x <- [1..50], '7' `elem` show x]

-- To see how filtering in list
-- comprehensions translates to the list monad, we have to check out the guard
-- function and the MonadPlus type class. The MonadPlus type class is for monads
-- that can also act as monoids. Here’s its definition:
instance (Monad m) => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- Because lists are monoids as well as monads, they can be
-- made an instance of this type class:
instance MonadPlus [] where 
    mzero = []
    mplus = (++)

-- The guard function is defined like this:
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

-- It takes a boolean value and if it’s True, takes a () and puts it in a minimal
-- default context that still succeeds. Otherwise, it makes a failed monadic value.
-- Here it is in action:
-- ghci> guard (5 > 2) :: Maybe ()
-- ghci> guard (5 > 2) :: [()]

-- In the list monad, we use it to filter out non-deterministic computations. Observe:
-- ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- Let’s first see how guard functions in conjunction with >>:
-- ghci> guard (5 > 2) >> return "cool" :: [String]
-- ghci> guard (1 > 2) >> return "cool" :: [String]

-- HEre is a previous example written in do notation 
sevensOnly :: [Int]
sevensOnly = do 
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

-- So filtering in list comprehensions is the same as using guard.