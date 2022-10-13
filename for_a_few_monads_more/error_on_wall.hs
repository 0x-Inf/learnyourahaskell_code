-- The Either type can be seem also as a Monad with added failure context only unlike the Maybe monad the failure context for Either allows us to 
-- add some additional information for the failure 

-- It's monad instance from Control.Monad.Error is as follows 
instance (Error e) => Monad (Either e) where
    return x = Right x 
    Right x >>= f = f x
    Left err >>= f = Left err 
    fail msg = Left (strMsg msg)

-- When a pattern match fails in do notation, a Left value is used to signify this failure.
-- A few examples of usage 
-- ghci> Left "boom" >>= \x -> return (x+1)
-- ghci> Right 100 >>= \x -> Left "no way!"


-- You could implement the tight rope walker with the Error Monad instead of the Maybe one, that remembers how many birds were on the poles when he fell 
