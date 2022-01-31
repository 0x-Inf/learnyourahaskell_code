{-# LANGUAGE InstanceSigs #-}

import Control.Monad.State
import Control.Monad
import Control.Applicative

-- Haskell features a thing called the state monad, which makes dealing with
-- stateful problems a breeze while still keeping everything nice and pure.

-- When we were dealing with random numbers, we dealt with functions that took
-- a random generator as a parameter and returned a random number and a new
-- random generator

-- a fn that takes StdGen and tosses a coin three times based on that generator. Like this 
{-
threeCoins ::  StdGen -> (Bool, Bool, Bool)
threeCoind gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
-}
-- Haskell is pure, we can’t reuse the generator, so we had to take some state, make a result
-- from it and a new state and then use that new state to generate new results.

-- In haskell we don't have to give the purity in order to get stateful functionality. 
-- since there exist a special little monad called the state monad which handles all this
-- state business for us and without giving up any of the purity that makes Haskell
-- programming so cool.

-- We’ll say that a stateful computation is a function that takes some state and returns a value along with some new state.
-- Like so 
-- s -> (a,s)

-- This stateful computation, a function that takes a state and returns a result and
-- a new state, can be thought of as a value with a context as well.

-- Stacks and stones 

-- Say we want to model operating a stack. You have a stack of things one on
-- top of another and you can either push stuff on top of that stack or you can
-- take stuff off the top of the stack.

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

-- Let’s write a small piece of code to simulate a stack using these functions. We’ll
-- take a stack, push 3 to it and then pop two items, just for kicks.

stackManip :: Stack -> (Int, Stack)
stackManip stack = let 
    ((), newStack1) = push 3 stack
    (a , newStack2) = pop newStack1
    in pop newStack2

-- Notice how stackManip is
-- itself a stateful computation. We’ve taken a bunch of stateful computations and
-- we’ve sort of glued them together.

-- The above way of maintaning state is tedious so we introduce a method below that alleviates that 

-- The State Monad 

-- The Control.Monad.State module provides a newtype that wraps stateful computations.
-- Here is the definition 
newtype State' s a = State' { runState' :: s -> (a,s) }

-- A State s a is a stateful computation that manipulates a state of type s and has a result of type a 

-- This is the Monad instance 

instance Functor (State' s) where 
    fmap = liftA 

instance Applicative (State' s) where
    pure a = State' $ \ s -> (a, s)

    (<*>) = ap

instance Monad (State' s) where 
    return x = State' $ \ s -> (x,s)
    (State' h) >>= f = State' $ \s -> let (a, newState) = h s
                                          (State' g) = f a
                                      in g newState 

-- So with >>=, we kind of glue two stateful computations together, only the second one is hidden inside a function that takes the previous one’s result.
-- Every time make sure you understand the above implementation .. especially the >>= part (remember all we want is stateful computations)


-- The pop and push fns using the State Monad 
pop' :: State' Stack Int 
pop' = State' $ \(x:xs) -> (x,xs)

push' :: Int -> State' Stack ()
push' a = State' $ \xs -> ((),a:xs) 

stackManip' :: State' Stack Int 
stackManip' = do 
    push' 3
    a <- pop' 
    pop' 

-- Another way we could have written this without binding the a to pop' since we didn't use it at all 
stackManip'' :: State' Stack Int 
stackManip'' = do 
    push' 3
    pop' 
    pop'

-- Here another with a bit more involved 
stackStuff :: State' Stack ()
stackStuff = do 
    a <- pop'
    if a == 5
        then push' 5
        else do 
            push' 3 
            push' 8 


-- Remember, do expressions result in monadic values and with the State monad,
-- a single do expression is also a stateful function.

moreStack :: State' Stack ()
moreStack = do 
    a <- stackManip''
    if a == 100
        then stackStuff
        else return ()

-- The Control.Monad.State module provides a type class that’s called
-- MonadState and it features two pretty useful functions, namely get and put.
-- For State, the get function is implemented like this:

get' = State' $ \s -> (s,s)
-- So It just takes the current state and presents it as a result 

-- The put fn takes some state and makes a stateful fn that replaces the current state with it
put' newState = State' $ \s -> ((), newState)

-- With these we can see what the current stack is or replace it with a new one 

stackyStack :: State' Stack () 
stackyStack = do 
    stackNow <- get' 
    if stackNow == [1,2,3]
        then put' [8,3,1]
        else put' [9,2,1]

-- We can examine what the type of >>= would be if only it worked for State values
-- (>>=) :: State' s a -> (a -> State' s b) -> State' s b

-- compared to Maybe's implementation of >>= where we are using one Maybe from one computation to the other 
-- Well, for the state monad, the
-- monad is actually State s, so if that s was different, we’d be using >>= between
-- two different monads.



