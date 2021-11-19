-- Implementing a function to find the maximum in a list recursively
maximum' :: (Ord a ) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Another way using the max function
maximum'' :: (Ord a ) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- Replicate function which makes n copies of a number x into a list
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0     = []
    | otherwise  = x:replicate' (n-1) x

-- Take function that takes n items from a list
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ []    = []
take' n (x:xs) = x : take' (n-1) xs

-- Reverse 
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat takes an element and returns an infinite list with just that element
repeat' :: a -> [a]
repeat' x = x: repeat' x

-- zip takes two lists and zips them together
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y): zip' xs ys

-- Elem checks if an element is in a list
elem' :: (Eq a) => a -> [a] -> Bool 
elem' a [] = False 
elem' a (x:xs)
    | a == x     = True 
    | otherwise  = a `elem'` xs

-- Quicksort implementation in haskell. Sort of the poster child for the language
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted



