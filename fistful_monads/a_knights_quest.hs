-- Say you have a chess board and only one knight piece on it. We want to find out
-- if the knight can reach a certain position in three moves.

import Control.Monad (guard)

type KnightPos = (Int, Int)

-- Here’s a function that takes the knight’s position and returns all of its next moves:
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

-- This function can also be written without the use of lists as a monad, but we
-- did it here just for kicks. Here is the same function done with filter:
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

-- Here’s a function that takes a
-- position and returns all the positions that you can reach from it in three moves:
in3 :: KnightPos -> [KnightPos]
in3 start = do 
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second 

-- The above without do notation:
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- Now, let’s make a function that takes two positions and tells us if you can get
-- from one to the other in exactly three steps:
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- As an exercise, you can change this function so that when you can reach
-- one position from the other, it tells you which moves to take.