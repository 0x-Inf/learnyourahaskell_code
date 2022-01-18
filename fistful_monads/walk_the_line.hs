-- We're going to simulate pierre the tight rope walker whose balancing pole can sometimes be occupied by birds which under certain conditions may 
-- throw him off-balance


type Birds = Int 
type Pole = (Birds, Birds)

-- a function that takes a number of birds and lands them on one side of the pole 

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left +n, right)

landRight :: Birds -> Pole -> Pole 
landRight n (left, right) = (left, right + n)

-- If we make a function like this 
x -: f = f x

-- We can apply functions by first writing the parameter and then the function 
-- ghci> 100 -: (*3)
-- ghci> True -: not 
-- ghci> (0,0) -: landLeft 2

-- ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft 2

-- we'll update the landing fns to fail on some certain inputs 
landLeft' :: Birds -> Pole -> Maybe Pole 
landLeft' n (left, right) 
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole 
landRight' n (left, right) 
    | abs (left- (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- ghci> landLeft' 2 (0,0)
-- ghci> landLeft' 10 (0,3)

-- With this new functions we need a way to use the Maybe Pole result and use it with a fn that takes a Pole, we'll make use of >>=
-- ghci> landLeft' 1 (0,0) >>= landLeft' 2

-- Here is a sequence of bird landings 
-- ghci> return (0,0) >>= landRight' 2 >>= landLeft' 2 >>= landRight' 2
-- ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

-- We can also make a function that ignores the current number of birds on the Pole and just makes pierre fall 
banana :: Pole -> Maybe Pole 
banana _ = Nothing 

-- check it 
-- ghci> return (0,0) >>= landRight' 2 >>= landLeft' 1 >>= banana >>= landRight' 2

-- Instead of making functions that ignore their input and just return a predetermined monadic value, we can use the >> function
(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n 

-- Here's how to see how it acts with Maybe
-- ghci> Nothing >> Just 3
-- ghci> Just 3 >> Just 4
-- ghci> Just 3 >> Nothing 

-- we can use >> instead of banana
-- ghci> return (0,0) >>= landLeft' 1 >> Nothing >>= landRight' 1

-- We can look at how we would have implemented a series of landings without the use of Maybe values with a failure context
routine :: Maybe Pole 
routine = case landLeft' 1 (0,0) of 
    Nothing -> Nothing 
    Just pole1 -> case landRight' 4 pole1 of
        Nothing -> Nothing 
        Just pole2 -> case landLeft' 2 pole2 of 
            Nothing -> Nothing 
            Just pole3 -> landLeft' 1 pole3

