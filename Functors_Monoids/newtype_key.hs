-- We can make an existing type an instance of a type class
-- We can use the data keyword to just wrap that type into another type 

-- The newtype keyword is made for when we want to just take one type and wrap it in something to present it as another type
-- the newtype keyword is suitable for this as it removes the overheads involved in doing the same using the data keyword and hence faster
-- for more info on this go to s
-- We can't just use the newtype keyword all the time instead of the data keyword as the newtype is restricted to one constructor with one field

-- we can also use the deriving keyword with the newtype keyword
newtype CharList  = CharList { getCharList :: [Char] } deriving (Eq, Show)
-- e.g
-- ghci> CharList "this will be shown!"
-- ghci> CharList "benny" == CharList "benny"
-- in this particular newtype, the value constructor has the following type
-- CharList :: [Char] -> CharList
-- the getCharList which was created since we used record syntax
-- getCharList :: CharList -> [Char]

-- using newtype to make type class instances 
-- Now what if we wanted to make the tuple an instance of Functor in such a way that when we fmap a function over a tuple,
--     it gets applied to the first component of the tuple
-- we can't use Maybe for this so 
-- to get around this,we can newtype our tuple in such a way that the second type parameter represents the type of the first component in the tuple: 
newtype Pair b a = Pair { getPair :: (a,b) }

-- And now, we can make it an instance of Functor so that the function is mapped over the first component: 
instance Functor (Pair c) where 
    fmap f (Pair (x,y)) = Pair (f x , y)

-- e.g
-- ghci> getPair $ fmap (*100) (Pair (2,3))
-- ghci> getPair $ fmap reverse (Pair  ("Getting real", 3))


-- Exploring the laziness of the newtype keyword
-- it's not only faster but also lazier

-- Now consider the following type:
-- data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- let's now use newtype 
newtype CoolBool = CoolBool { getCoolBool :: Bool }

-- the newtype helloMe runs because when we define the CoolBool haskell doesn't have to evaluate it since implicitly haskell knows that 
-- newtype only takes one constructor and one field


-- type vs newtype vs data
-- type keyword is simply making a more desriptive synonym for an existing type e.g
type IntList = [Int]
-- ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])

-- newtype keyword is used for taking existing types and wrapping them in a new types,
-- so that's it's easier to make them instances of certain type classes
-- When we use newtype to wrap an existing type, the type that we get is separate from the original type. If we make the following newtype: 
newtype CHarList = CHarList {getCHarList :: [Char]} 
-- When we use record syntax in our newtype declarations, we get functions for converting between the new type and the original type
-- In practice, you can think of newtype declarations as data declarations that can only have one constructor and one field

-- data keyword is for making you data types and with them you can go hog wild 
-- They can have as many constructors and fields as you wish and can be used to implement any algebraic data type by yourself. 
