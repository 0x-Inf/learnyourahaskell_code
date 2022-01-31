-- Were going to look at a few functions that either operate on monadic values or return monadic values as their results (or both!)

-- liftM and friends 
-- even though every monad is a functor, we don’t have to rely on it having a Functor instance because of the liftM function.
-- liftM takes a function and a monadic value and maps it over the monadic value. So it’s pretty much the same thing as fmap!

liftM :: (Monad m) => (a -> b) -> m a -> m b
-- and this is the type of fmap 
fmap :: (Functor f) => (a -> b) -> f a -> f b

-- Examples of liftM 
-- ghci> liftM (*3) (Just 8)
-- ghci> runWriter $ liftM not $ Writer (True, "chickpeas")
-- ghci> runState (liftM (+100) pop) [1,2,3,4]

-- Doing fmap or liftM over a stateful
-- computation results in another stateful computation, only its eventual result is
-- modified by the supplied function.

-- This is how liftM is implemented 
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return (f x))

-- or with do notation 
liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do 
    a <- m 
    return f a

-- The Applicative type class allows us to apply functions  between values with contexts as if they were normal values 
-- ghci> (*) <$> Just 3 <*> Just 14

-- Using this applicative style makes things pretty easy. <$> is just fmap and <*>
-- is a function from the Applicative type class that has the following type:

(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- So it's kind of like fmap, only the function itself is in a context.
-- We have to somehow extract it from the context and map it over the f a value and then assemble the context back together
-- we can use the combination of <$> and <*> to apply functions that take several parameters between applicative values.

-- The ap function is basically <*>, only it has a Monad constraint instead of an Applicative one. Here’s its definition:
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do 
    f <- mf
    x <- m 
    return (f x)
-- mf is a monadic value whose result is a function.

-- some examples 
-- ghci> Just (+3) <*> Just 4
-- ghci> Just (+3) `ap` Just 4
-- ghci> [(+1),(+2),(+3)] <*> [10,11]
-- ghci> [(+1),(+2),(+3)] `ap` [10,11]

-- when a type is found to be a monad, people first write up
-- a Monad instance and then make an Applicative instance by just saying that
-- pure is return and <*> is ap. Similarly, if you already have a Monad instance for
-- something, you can give it a Functor instance just saying that fmap is liftM.

-- The liftA2 function is a convenience function for applying a function between two applicative values.Defined as:
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- The liftM2 function does the same thing, only it has a Monad constraint. There also exist liftM3 and liftM4 and liftM5.


-- The Join Function 

-- If the result of one monadic value is another monadic value i.e. if one monadic value is nested inside the other, can you flatten them to just a single normal monadic value?
-- It turns out that any nested monadic value can be flattened and that is actually a property unique to monads 
-- For this the join fn exists 
join :: (Monad m) => m (m a) -> m a

-- So it takes a monadic value within a monadic value and gives us just a monadic value, so it sort of flattens it.
-- Some example with Maybe 
-- ghci> join (Just (Just 9))
-- ghci> join (Just Nothing)
-- ghci> join Nothing 

-- Flattening lists is pretty intuitive 
-- ghci> join [[1,2,3], [4,5,6]]

-- To flatten a Writer value whose result is a Writer value itself, we have to mappend the monoid value.
-- ghci> runWriter $ join (Writer (Writer (1, "aaa"), "bbb"))

-- Intuitively speaking, when you want to examine what the result of a Writer
-- value is, you have to write its monoid value to the log first and only then can
-- you examine what it has inside.

-- Flattening Either values is very similar to flattening Maybe values
-- ghci> join (Right (Right 9))
-- ghci> join (Right (Left "error"))

-- If we apply join to a stateful computation whose result is a stateful computation, the result is a stateful computation that runs first the outer 
-- stateful computation and then the resulting one 
-- ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]

-- The lambda here takes a state and puts 2 and 1 onto the stack and presents push 10 as its result. So when this whole thing is flattened with join and then run,
-- it first puts 2 and 1 onto the stack and then push 10 gets carried out, pushing a 10 on to the top.

-- The do implementation for join is as follows 
join' :: (Monad m) => m (m a) -> m a
join' mm = do 
    m <- mm 
    m 

-- Perhaps the most interesting thing about join is that for every monad, feeding
-- a monadic value to a function with >>= is the same thing as just mapping that function over the value and 
-- then using join to flatten the resulting nested monadic value! 

-- The fact that m >>= f always equals join (fmap f m) is very useful if we’re
-- making our own Monad instance for some type because it’s often easier to figure
-- out how we would flatten a nested monadic value than figuring out how to
-- implement >>=.

