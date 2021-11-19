-- Demonstration of currying where we create another function in the parameters of a fn to add more params (maybe, its weird)
multiThree :: (Num a) => a -> a -> a -> a
multiThree x y z  = x * y * z


-- By calling functions with too few parameters, so to speak, we're creating new functions on the fly.
-- ghci> let multTwoWithNine = multThree 9
-- ghci> multTwoWithNine 2 3
-- ghci>let multWithEighteen = multTwoWithNine 2  
-- ghci> multWithEighteen 10  

-- A function that compares a number with 100
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred  = compare 100 
-- compareWithHundred x = compare 100 x. Think about what this evaluates to when x is 100 


-- Infix functions can also be partially applied by using sections.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
--That creates a function that takes one parameter and then applies it to the side that's missing an operand

-- A function that checks if a character supplied to it is an uppercase letter
isUpperAlphanum :: Char -> Bool 
isUpperAlphanum = (`elem` ['A'..'Z'])


-- Functions can take functions as parameters and also return functions
--we're going to make a function that takes a function and then applies it twice to something!
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Implementation of zipWith: It takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
--Example: zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

--Flip: It simply takes a function and returns a function like the og one but the 1st 2 arguments are flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y
-- Example : zipWith (flip'' div) [2,2..] [10, 8, 6, 4, 2]


--Map: takes a function and a list and applies that function to every element in the list, producing a new list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- Example: map (map (^2)) [[1,2],[3,4,5,6],[7,8]] 


--Filter: is a function that takes a predicate and a list and then returns the list of elements that satisfy the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
-- Example : filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  

-- Quicksort but with filter
quicksort'' ::(Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = 
    let smallerSorted = quicksort'' (filter (<=x) xs)
        biggerSorted = quicksort'' (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

--Let's find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3929 == 0

--  find the sum of all odd squares that are smaller than 10,000
-- makes use of takeWhile  It takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true
-- Once an element is found for which the predicate doesn't hold, it stops
--ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- Using list comprehensions
-- ghci> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])


-- Collatz sequences
-- We take a natural number. If that number is even, we divide it by two
-- If it's odd, we multiply it by 3 and then add 1 to that.
-- We take the resulting number and apply the same thing to it, which produces a new number and so on.
-- We want to know for all starting numbers between 1 and 100, how many chains have a length greater than 15?
chain :: (Integral a) => a -> [a]
chain 0 = error "This will break the computer!! haha"
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- numLong chains with a lambda fn instead of where
numLongChains' :: Int 
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..1000]))

-- lambda fns can take any number of params 
-- example ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5] 
-- one can also pattern match with lamdas although one can't define several patters for one parameter, errors abound
-- example ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- Here are some interesting fns one is 'normal' the other is showing off currying and lambda fns
addThree :: (Num a) => a -> a -> a -> a
addThree x y z  = x + y + z
-- equivalent to
addThree' :: (Num a ) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

-- Another way to define flip using lambdas
flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x


--Fold  fold takes a binary function, a starting value (I like to call it the accumulator) and a list to fold up.
--The binary function itself takes two parameters. The binary function is called with the accumulator and the first (or last) element and produces a new accumulator.
--Then, the binary function is called again with the new accumulator and the now new first (or last) element, and so on.
--Once we've walked over the whole list, only the accumulator remains, which is what we've reduced the list to.

-- We'll implement the sum fn using the foldl
sum' :: (Num a)  => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- Another implementetion of sum using fold and currying
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

--Elem using fold
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

--foldr: similar to the foldl but consumes values from the right side
-- We'll implement map using the right fold
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- Some other standard lib fn using folds
-- Foldr1 is like foldr but the 'input' doesn't have to be defined makes use of currying
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr(\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1(\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- Scan: scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list
-- Scans are used to monitor the progression of a function that can be implemented as a fold
-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1



-- Function application with $ fn
--definition
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
--Example ghci> map ($ 3) [(4+), (10*), (^2), sqrt]

-- Function composition, just like in mathematics
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
-- Examples : ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
--            ghci>sum . replicate 5 . max 6.7 $ 8.9 equivalent to sum . replicate 5 . max 6.7 $ 8.9
--            ghci>replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

--In more readable fashion
oddSquareSum' :: Integer 
oddSquareSum' = 
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit

