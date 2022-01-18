-- Just like functors have the Functor type class and applicative functors have the Applicative type class.
-- monads come with their own type class: Monad! This is what it looks like 
class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y

    fail :: String -> m a
    fail msg = error msg

-- return here takes a value and wraps it in a context 
-- the bind function takes a monad, a function that takes a 'normal' value and gives a contextualized (Monadic) value and gives the contextualized value
-- the fail function is used by haskell to enable failure in a special syntactic construct for monads that we'll meet later. 


-- Now that we know what the Monad type class looks like, let's take a look at how Maybe is an instance of Monad! 
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing

-- The return is just the same as pure from applicative functors 
-- the bind is the same as the applyMaybe that we saw earlier Nothing begets Nothing and If it’s a Just we take what’s inside and apply f to it.
-- On ghci we can play with this 
-- ghci> return "WHAT!!" :: Maybe String
-- ghci> Just 9 >>= \x -> return (x*19)


