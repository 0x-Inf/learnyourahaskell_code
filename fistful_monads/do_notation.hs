-- Monads in Haskell are so useful that they got their own special syntax called do notation.
-- We've seen previously that we can use do notation to chain IO actions together, 
-- well as it turns out, do notation isn’t just for IO, but can be used for any monad.
-- gluing together monadic values in sequence

-- Now, what if we had another >>= inside that function? Check this out:
-- ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

-- Ah, a nested use of >>=! In the outermost lambda, we feed Just "!" to the lambda \y -> Just (show x ++ y). Inside this lambda, the y becomes "!". x
-- is still 3 because we got it from the outer lambda
-- this sort of looks like the following expression 
-- ghci> let x = 3 ; y = "!" in show x ++ y
-- it's only in the former case that we have monadic values with a failure context
-- ghci> Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- ghci> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))
-- ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))

--let’s write this in a script and have each Maybe value take up its own line:
foo :: Maybe String 
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- Haskell gives us the do notation s.t in this case we can avoid writing the annoying lambdas
foo' :: Maybe String 
foo' = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- It’s important to remember that do expressions are just different syntax for chaining monadic values.
-- In a do expression every line is a monadic value. To inspect its result, we use <-
-- The last monadic value in a do expression, like Just (show x ++ y) here, can’t be used with <- to bind its result, because that wouldn’t make sense
-- if we translated the do expression back to a chain of >>= applications.
-- Its result is the result of the whole glued up monadic value, taking into account the possible failure of any of the previous ones.

-- for instance
-- ghci> Just 9 >>= (\x -> Just (x > 8))
-- rewriting this in do notation we get 
marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)


-- Our tightwalker’s routine can also be expressed with do notation.
type Birds = Int 
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole 
landLeft n (left, right) 
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole 
landRight n (left, right) 
    | abs (left- (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

-- If we want to throw the Pierre a banana peel in do notation, we can do the following:

routine' :: Maybe Pole 
routine' = do 
    let start = (0,0)
    first <- landLeft 2 start 
    Nothing 
    second <- landRight 2 first 
    landLeft 1 second 


--Here’s an example of pattern matching in a do expression

justH :: Maybe Char 
justH = do 
    (x:xs) <- Just "Hello"
    return x

-- Here’s a do expression with a pattern that’s bound to fail:
wopwop :: Maybe Char
wopwop = do 
    (x:xs) <- Just ""
    return x

-- The failed pattern matching has caused a failure within the context of our monad instead of causing a program-wide failure, which is pretty neat.

