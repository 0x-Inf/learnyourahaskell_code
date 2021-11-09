module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
import qualified Data.Map as Map

-- So far, we've run into a lot of data types. Bool, Int, Char, Maybe
-- But how to make ours. Well, one way is to use the data keyword to define a type
-- Lets see how the Bool type is defined in the standard lib
-- data Bool = False | True 
-- data means that we're defining a new data type
-- The part before the = denotes the type, which is Bool. The parts after the = are value constructors

-- We can think of Int type  being defined as (but not really)
-- data Int =  -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647

-- A type to represent shape which can be a circle or rectangle
-- the deriving keyword makes out Shape type part of Show type
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- Cool, so value constructors are functions like everything else

-- A function that takes a shape and returns its surface
-- surface :: Shape -> Float 
-- surface (Circle _ _ r) = pi * r * 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2) 

-- Value constructors are functions, so we can map them and partially apply them and everything
-- a list of concentric circles with different radii
-- ghci> map (Circle 10 20) [4,5,6,6]

-- A better implementation of the shape data type
--  Let's make an intermediate data type that defines a point in two-dimensional space.
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)


-- The new surface fn
surface :: Shape -> Float 
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- A fn that nudges a shape 
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- If we don't want to deal directly with points,
-- we can make some auxilliary functions that create shapes of some size at the zero coordinates and then nudge those.
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- we've been tasked with creating a data type that describes a person.
-- data Person = Person String String Int Float String String deriving (Show)
-- ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- What if we wanted to get specific details about the person
-- One way is to create fn for the specific details we want
-- or we could use record syntax like so:
data Person = Person {
      firstName :: String
    , lastName :: String 
    , age :: Int 
    , height :: Float 
    , phoneNumber :: String 
    , flavor :: String 
    }deriving (Show)
-- By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor.

-- Another benefit is When we derive Show for the type, it displays it differently if we use record syntax to define and instantiate the type
data Car' = Car' String String Int deriving (Show)

-- Using record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

--When making a new car, we don't have to necessarily put the fields in the proper order, as long as we list all of them.

-- Type parameters e.g
-- data Maybe a  =  Nothing | Just a
-- Here a is the type parameter

-- Type parameters are useful because we can make different types with them depending on what kind of types we want contained in our data type.

-- PS: don't put type constraints into data declarations even if it seems to make sense, because you'll have to put them into the function type declarations either way.

-- Let's implement a 3D vector type and add some operations for it.
-- We'll be using a parameterized type because even though it will usually contain numeric types, it will still support several of them.
data Vector a  =  Vector a a a deriving (Show)

vPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vPlus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n


-- let's see how Haskell can automatically make our type an instance of any of the following typeclasses: Eq, Ord, Enum, Bounded, Show, Read
-- Haskell can derive the behavior of our types in these contexts if we use the deriving keyword when making our data type.

data PersonS  = PersonS { firstNameS :: String
                        , lastNameS :: String 
                        , ageS :: Int
                        } deriving (Eq, Show, Read)


-- We can derive instances for the Ord type class, which is for types that have values that can be ordered
-- If we compare two values of the same type that were made using different constructors,
--the value which was made with a constructor that's defined first is considered smaller
-- For instance we can think of it as being implemented like this:
-- data Bool = False | True deriving (Ord)

--We can easily use algebraic data types to make enumerations and the Enum and Bounded typeclasses help us with that
data Day = Monday | Tuesday | Wednesday | Thurday | Friday | Saturday | Sunday 
            deriving (Eq, Ord, Show, Read, Bounded, Enum) 

-- ghci> [minBound .. maxBound] :: [Day]


-- we mentioned that when writing types, the [Char] and String types are equivalent and interchangeable
-- That's implemented with type synonyms
-- Type synonyms don't really do anything per se, they're just about giving some types different names so that they make more sense to someone reading our code and documentation.
-- E.g here's how the standard lib defines String as a synonym for [Char] 
-- type String = [Char]

phoneBook :: [(String, String)]
phoneBook = 
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")
    ]


-- type PhoneBook = [(String, String)]
type PhoneNumber = String 
type Name = String 
type PhoneBook = [(Name, PhoneNumber)]

-- Giving the String type synonyms is something that Haskell programmers do when they want to convey more information
-- about what strings in their functions should be used as and what they represent.
-- We can use the types above in really nice to read fns
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Type synonyms can also be parameterized.
-- If we want a type that represents an association list type but still want it to be general so it can use any type as the keys and values
type AssocList k v = [(k,v)]

-- Another cool data type that takes two types as its parameters is the Either a b type. This is roughly how it's defined
-- data Either a b  =  Left a | Right b deriving (Eq, Ord, Read, Show)
-- It has two value constructors. If the Left is used, then its contents are of type a and if Right is used, then its contents are of type b


-- Example of High School locker system 
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String 

type LockerMap = Map.Map Int (LockerState, Code)

-- we're going to make a function that searches for the code in a locker map
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                 then Right code
                                 else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList 
    [(100, (Taken, "ZD39I"))
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]


-- Recursive data structures
-- Using algebraic data types to implement our own list
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- In record syntax for clarity 
-- data List a = Empty | Cons { listhead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)

-- We can define functions to be automatically infix by making them comprised of only special characters.
-- We can also do the same with constructors, since they're just functions that return a data type.
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- Let's make a function that adds two of our lists together. This is how ++ is defined for normal lists:
-- infixr 5 ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

-- For our custom list addition we do this
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys  = x :-: (xs .++ ys)


-- Implementing a binary search tree
data Tree a  = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x 
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem x EmptyTree = False 
treeElem x (Node a left right)
    | x == a = True 
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- Typeclasses
-- A typeclass defines some behavior (like comparing for equality, comparing for ordering, enumeration)
-- and then types that can behave in that way are made instances of that typeclass. 
-- E.g this is how the Eq class is defined in the standard prelude
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True 
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green light"

-- You can also make typeclasses that are subclasses of other typeclasses.
-- Here is the first part of the Num typeclass
-- class (Eq a) => Num a where
-- That's all there is to subclassing really, it's just a class constraint on a class declaration!

-- We've have to use concrete types when subclassing. E.g
-- instance Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False

-- A problem with the above declaration is that e use == on the contents of the Maybe but we have no assurance that what the Maybe contains can be used with Eq!
-- we have to modify our instance declaration like this:
-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False


-- Some weakly typed languages (Javascript) have truthy valuations for types other than Bool e.g empty string is 'false'
-- Let's try and implement similar functionality in haskell for fun of course
class YesNo a where
    yesno :: a -> Bool

-- let's define some instances. For numbers, 
-- we'll assume that (like in JavaScript) any number that isn't 0 is true-ish and 0 is false-ish.

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False 
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False 
    yesno _ = True

-- Let's make a function that mimics the if statement, but it works with YesNo values.
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult


-- The functor typclass (yaaay go CT)
-- Functor typeclass, which is basically for things that can be mapped over
-- how it's implemented
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- here's how the list is an instance of the Functor typeclass.
instance Functor' [] where
    fmap' = map

-- Here is how Maybe is a Functor
instance Functor' Maybe where
    fmap' f (Just x) = Just (f x)
    fmap' f Nothing = Nothing


-- We can also make our Tree type an instance of the Functor class
instance Functor' Tree where 
    fmap' f EmptyTree = EmptyTree
    fmap' f (Node x leftsub rightsub) = Node (f x) (fmap' f leftsub) (fmap' f rightsub)

-- Here's how Either a is a functor in the standard libraries:
instance Functor' (Either a) where
    fmap' f (Right x)  = Right (f x)
    fmap' f (Left x) = Left x

-- We'll try and make Map k is made an instance of functor
-- instance Functor' (Map.Map k) where
--     fmap' f (b a) =  k (f v)




