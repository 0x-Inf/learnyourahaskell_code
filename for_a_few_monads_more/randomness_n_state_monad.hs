import System.Random
import Control.Monad.State
import Control.Monad
import Control.Applicative


-- The state Monad makes dealing with a random number generator easier instead of having to generate a new one every time 

-- The random fn from System.Random has the following type
-- random :: (Random g, Random a) => g -> (a, g)

-- We can see that it’s a stateful computation, so we can wrap it
-- in the State newtype constructor and then use it as a monadic value so that
-- passing of the state gets handled for us:
randomSt :: (RandomGen g, RandomGen a) => State g a
randomSt = state random 

-- So now if we want to throw three coins (True is tails, False is heads) we just do the following:

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do 
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)


-- threeCoins is now a stateful computations and after taking an initial random
-- generator, it passes it to the first randomSt, which produces a number and a new
-- generator, which gets passed to the next one and so on.

-- ghci> runState threeCoins (mkStdGen 33)