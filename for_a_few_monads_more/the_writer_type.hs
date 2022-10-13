import Control.Monad.Writer 

-- Now we've seen that a value with an attached monoid acts like a monadic value.
-- Let's examine the monad instance for types of such values 

-- Let's examine the type itself.
-- To attach a monoid to a value, we just need to put them together in a tuple
--The Writer w a type is just a newtype wrapper for this. Its definition is very simple:
{-
newtype Writer w a = Writer {runWriter :: (a, w) }
-}
-- It's Monad instance is defined like so :
{-
instance (Monoid w) => Monad (Writer w) where 
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-}

-- When you have to poop, poop, don't talk

-- ghci> runWriter (return 3 :: Writer String Int)
-- ghci> runWriter (return 3 :: Writer (Sum Int) Int)
-- ghci> runWriter (return 3 :: Writer (Product Int) Int)


-- Using do notation 

-- Now that we have a Monad instance, we’re free to use do notation for Writer
-- values. It’s handy for when we have a several Writer values and we want to do
-- stuff with them.

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do 
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)

-- ghci> runWriter multWithLog

-- Sometimes we just want some monoid value to be included at some particular point. For this, the tell function is useful.
-- It’s part of the MonadWriter type
-- class and in the case of Writer it takes a monoid value, like ["This is going
-- on"] and creates a Writer value that presents the dummy value () as its result
-- but has our desired monoid value attached.

multWithLog' :: Writer [String] Int
multWithLog' = do 
    a <- logNumber 3 
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

    