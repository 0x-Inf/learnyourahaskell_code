-- Just like applicative functors, and functors before them, monads come with a
-- few laws that all monad instances must abide by.

-- Left identity 
-- The first monad law states that if we take a value, put it in a default context
-- with return and then feed it to a function by using >>=, it’s the same as just
-- taking the value and applying the function to it. To put it formally:

-- return x >>= f is the same damn thing as f x
-- e.g
-- ghci> return 3 >>= (\x -> Just (x + 100000))
-- ghci> (\x -> Just (x + 100000)) 3

-- For the list monad
-- ghci> return "WoM" >>= (\x -> [x,x,x])
-- ghci (\x -> [x,x,x]) "WoM"

-- We said that for IO, using return makes an I/O action that has no side-effects
-- but just presents a value as its result. So it makes sense that this law holds for
-- IO as well.

-- Right Identity 
-- The second law states that if we have a monadic value and we use >>= to feed it
-- to return, the result is our original monadic value. Formally:

-- m >>= return is no different than just m

-- Here's a test run for a few monads
-- ghci> Just "move on up" >>= (\x -> return x)
-- ghci> [1,2,3,4] >>= (\x -> return x)

-- Left identity and right identity are basically laws that describe how return
-- should behave. It’s an important function for making normal values into monadic
-- ones and it wouldn’t be good if the monadic value that it produced did a lot of
-- other stuff.

-- Associativity 
-- The final monad law says that when we have a chain of monadic function
-- applications with >>=, it shouldn’t matter how they’re nested. Formally written:

-- Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)

-- so let’s take a look at an example that makes this equality a bit clearer.
-- ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2 
-- ghci> ((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2

-- So it doesn’t matter how you nest feeding values to monadic functions, what matters is their meaning.

-- Another way to see it is by considering fn composition 
-- we can compose two monadic functions:

(<==<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <==< g = \x -> g x >>= f

-- So now we can compose two monadic functions

-- ghci> let f x = [x,-x]
-- ghci> let g x = [x*3, x*2]
-- ghci> let h = f <=< g
-- ghci> h 3
