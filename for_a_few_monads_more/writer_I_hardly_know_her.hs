import Data.Monoid
-- Whereas MAybe is for values that contain the context of possible failure and Lists containt the context of non-determinism 
-- The Writer Monad is for values that have another value attached to them like a log or something 
-- Writer allows us to do computations while making sure that all the log values are combined into one log value that then gets attached to the result. 

-- For instance, we might want to equip our values with strings that explain what’s
-- going on, probably for debugging purposes.
-- Check out this fn 
isBigGang :: Int -> Bool 
isBigGang x = x > 9

-- We can make it say something about what's its doing 
isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x >9 , "Compared the gang size to 9")

-- let’s make a function that takes a value with an attached log,
-- that is, an (a,String) value and a function of type a -> (b,String) and feeds
-- that value into the function. We’ll call it applyLog.

applyLog :: (Monoid m) => (a, m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

-- Examples:
-- ghci> (30, "A output of the cyber machine") `applyLog` isBigGang'
-- ghci> ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
-- ghci> ("X3p0", "Got Dipper name.") `applyLog` (\x -> (coolFactor x, "Checked alphanumeric pattern"))

-- coolFactor :: String -> (String, (String, String))
-- coolFactor xs = do 
--     x <- return xs
--     y <- [ 1 | y <- return xs, x == y ]
--     return (x, show y)

-- we no longer
-- have to think of the tuple as a value and a log, but now we can think of it as a
-- value with an accompanying monoid value.
-- here is a function that adds drinks to some cowboy food

type Food = String 
type Price = Sum Int 

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("Whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

-- ghci> ("beans", Sum 10) `applyLog` addDrink
-- ghci> ("jerky", Sum 25)  `appluLog` addDrink 
-- ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink