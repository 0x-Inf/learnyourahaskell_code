-- Monads are a natural extension of applicative functors and with them we are concerned with this:
-- If you have a value with a context m a how do you apply it to a function  that takes a normal a  and returns a value with a context?
-- In other words how do you apply a function of type a -> m b to a value of type m a?
-- Essentially we want this fn
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- If we have a fancy value and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function?
-- The >>= function is pronounced as bind. 

-- Maybe is a Monad
-- Let's think about how we would do >>= for Maybe 
-- Like we said >>= takes a monadic value, and a function that takes a normal value and returns a monadic value
-- and manages to apply that function to the monadic value
-- In this case, >>= would take a Maybe a value and a function of type a -> Maybe b and somehow apply the function to the Maybe a

--  Let's say that we have a function \x -> Just (x+1). It takes a number, adds 1 to it and wraps it in a Just: 
-- ghci> (\x -> Just (x+1)) 1
-- ghci> (\x -> just (x+1)) 100
-- The above fn is straightforward, now here's the kicker: how do we feed a Maybe value to this function

-- Instead of calling it >>=, let's call it applyMaybe for now. 
-- It takes a Maybe a and a function that returns a Maybe b and manages to apply that function to the Maybe a
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- We'll use it as an infix function so that the Maybe value is on the left side and the function on the right: 
-- ghci> Just 3 `applyMaybe` \x -> Just (x+1) 
-- ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
-- ghci> Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing
-- ghci> Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing
