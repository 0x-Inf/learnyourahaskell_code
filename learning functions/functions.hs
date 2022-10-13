import Distribution.Simple.Utils (xargs)


-- Examples of pattern mathcing 
lucky :: (Integral a) => a -> String
lucky 7  = "LUCKY NUMBER SEVEN!!"
lucky x  = "Sorry, you're out of luck pal!!"


sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe x = "No between 1 and 5"

-- Using pattern matching for some recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

-- Always include a catch-all pattern so that our program doesn't crash if we get some unexpected input. 
charName :: Char -> String 
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- Adding vectors without pattern matching
addVect :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVect a b = (fst a + fst b, snd a + snd b)

-- with pattern matching
addVect' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVect' (x1, y1) (x2, y2) = (x1 + x2 , y1 + y2)

-- extracting components of a tuple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--pattern matching against lists
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!!"
head' (x:_) = x

tell :: (Show a) => [a] -> String 
tell [] = "THis list is empty"
tell (x:[]) = "This list has one element:" ++ show x
tell (x:y:[]) = "This list has two elements:" ++ show x ++ " And " ++ show y
tell (x:y:_) = "This list is long the first two elemnts are: " ++ show x ++ " and " ++ show y

-- length function using pattern matching and a little recursion
length' :: (Num b) => [a] -> b 
length' [] = 0
length' (_:xs) = 1 + length' xs

-- sum using pattern matching
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- a quick and dirty example on patterns, used to break smth into patterns while still keeping a reference to the whole thing
capital :: String -> String 
capital "" = "Empty string whooops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards, the're like if statements, for testing whether some property of a value is true or false added a where later 
bmiTell :: (RealFloat a) => a ->  a -> String 
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normaal"
    | bmi <= fat = "You're fat!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
          -- (skinny, normal, fat) = (18.5, 25.0, 30.0)  

--max function using guards
max' :: (Ord a ) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- compare using guards
myCompare :: (Ord a) => a -> a -> Ordering 
a `myCompare` b
    | a > b     = GT 
    | a == b    = EQ 
    | otherwise = LT


-- where bindinds: are a syntactic construct that let you bind to variables at the end of a function and the whole function can see them, including all the guards
initials :: String -> String -> String 
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


calcBmis :: (RealFloat a) => [(a,a)]-> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2


--let bindings: they let you bind to variables anywhere and are expressions themselves, but are very local, so they don't span across guards.
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- calcBmis using let
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- case expressions 
head'' :: [a] -> a
head'' xs  = case xs of [] -> error "No head for empty lists!"
                        (x:_) -> x

-- this is the syntax of a case expression 
-- case expression of pattern -> result  
--                    pattern -> result  
--                    pattern -> result 

-- Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere.
describeList :: [a] -> String 
describeList xs  = "The list is " ++ case xs of [] -> "empty"
                                                [x] -> "a singleton list"
                                                xs -> "a longer list"


describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

