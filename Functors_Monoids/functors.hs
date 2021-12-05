-- Let's see how IO is an instance of Functor

{-
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
--}


{-
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"
-}

-- here's how to rewrite using fmap
main  = do line <- fmap reverse getLine 
           putStrLn $ "You said " ++ line ++ " backwards!"
           putStrLn $ "Yes you really said " ++ line ++ " backwards!"


-- We'll explore some features of functions 
-- (->) r is an instance of functor 
-- Below is a illustration
{-
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
-}
-- further investigations on the above shows that it's very similar to function composition i.e
-- fmap :: (a -> b) -> (r -> a) -> (r -> b) takes a fn from a to b and a fn from r to a and returns a fn from r to b
-- another way to write the above instance is :
{-
instance Functor ((->) r) where
    fmap = (.)
-}

-- We can play with mapping over functions
-- ghci> :t fmap (*3) (+100)  
-- ghci> fmap (*3) (+100) 1
-- ghci> (*3) `fmap` (+100) $ 1
-- ghci> (*3) . (+100) $ 1

-- if we write fmap :: (a -> b) -> (f a -> f b)
-- we can think of fmap not as a function that takes one function and a functor and returns a functor
-- but as a function that takes a function and returns a new function that's just like the old one,
-- only it takes a functor as a parameter and returns a functor as the result.
-- this is called lifting a fn
-- we can play with ghci's :t 
-- ghci> :t fmap (*2)

-- You can think of fmap as either a function that takes a function and a functor and then maps that function over the functor,
-- or you can think of it as a function that takes a function and lifts that function so that it operates on functors

-- some more ghci examples
-- ghci> fmap (replicate 3) (Right "bar")
-- ghci> fmap (replocate 3) (Just 4)
-- ghci> fmap (replicate 3) [1,2,3,4]



-- Functor Laws
-- The first functor law states that if we map the id function over a functor, the functor that we get back should be the same as the original functor
-- more formally fmap id = id
-- let's see this law in action 
-- ghci> fmap id (Just 3)
-- ghci> id (Just 3)
-- ghci> fmap id [1..5]
-- ghci> id [1..5]

-- If we look at the implementation of fmap for, say, Maybe, we can figure out why the first functor law holds.
{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap Nothing = Nothing
-}

-- The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping
-- one function over the functor and then mapping the other one
-- formally that means fmap (f . g) = fmap f . fmap g
-- or fmap (f . g) F = fmap f (fmap g F) for any functor F


-- Let's take a look at a pathological example of a type constructor being an instance of the Functor typeclass but not really being a functor, because it doesn't satisfy the laws
-- Let's say we have
data CMaybe a = CNothing | CJust Int a deriving (Show)
-- we'll play with this type to get some intuition
-- ghci> CNothing
-- ghci> CJust 0 "haha"
-- ghci> CJust 100 [1,2,3]

--  Let's make this an instance of Functor so that everytime we use fmap, the function gets applied to the second field, whereas the first field gets increased by 1.
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter +1) (f x)

-- We can play with it a bit 
-- ghci> fmap (++"ha") (CJust 0 "ho")
-- ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
-- ghci> fmap (++"blah") CNothing

-- To find out if this doesn't obey the functor laws, one counter-example is enough
-- ghci> fmap id (CJust 0 "haha")
-- ghci> id (CJust 0 "haha") 