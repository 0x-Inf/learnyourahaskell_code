-- In the chapter about applicatives, we saw that the function type, (->) r is
-- an instance of Functor. Mapping a function f over a function g will make a
-- function that takes the same thing as g, applies g to it and then applies f to
-- that result.

-- We’ve also seen that functions are applicative functors. They allow us to operate
-- on the eventual results of functions as if we already had their results. Here’s an
-- example:
-- ghci> let f = (+) <$> (*2) <*> (+10)
-- ghci> f 3 

-- Just like other monadic values that we’ve met so far, a
-- function can also be considered a value with a context. The context for functions
-- is that that value is not present yet and that we have to apply that function to
-- something in order to get its result value.

-- Let's look at how the Monad instance of (->) looks like 
{-
instance Monad ((->) r) where 
    return x = \_ -> x
    h >>= f = \w -> f (h w) w
-}
-- Here's a simple example that utilizes this Monad 
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)


-- return (a+b) does as well, but it ignores it and always presents a+b as the result. For this
-- reason, the function monad is also called the reader monad. All the functions
-- read from a common source. To illustrate this even better, we can rewrite
-- addStuff like so:

addStuff' :: Int -> Int 
addStuff' x = let 
    a = (*2) x
    b = (+10) 
    in a+b

-- We see that the reader monad allows us to treat functions as values with a context.
-- We can act as if we know what the fns will return 

    