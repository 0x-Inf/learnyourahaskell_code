import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Geometry.Cube as Cube


numUniques :: (Eq a) => [a] -> Int
 -- we use fn composition this is equivalent to (\xs -> length (nub xs)) or (\xs -> length $ nub xs)
numUniques = length . nub 

-- One can also import modules in the ghci console by 
-- ghci> :m + Data.List Data.Map Data.Set
-- Importing while ignoring some fns in the module 
-- import Data.List hiding (nub)
-- To avoid clashes with fn in Prelude fn we import modules this way:
-- import qualified Data.Map as M

--Some more Data.List fns
-- intersperse: takes an element and a list and then puts that element in between each pair of elements in the list
-- ghci> intersperse '.' "MONKEY"
-- intercalate: takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.
-- ghci> intercalate [0,0,0] [[1,2,3], [4,5,6], [7,8,9]]
-- transpose: transposes a list of lists. If you look at a list of lists as a 2D matrix, the columns become the rows and vice versa.
-- ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]

-- Say we have the polynomials 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1 and we want to add them together
-- We can use the lists [0,3,5,9], [10,0,0,9] and [8,5,1,-1] to represent them in Haskell.
-- Now, to add them, all we have to do is this:
-- ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]

-- concat: flattens a list of lists into just a list of elements.
-- ghci> concat ["foo", "bar", "car"]
-- concatMap: is the same as first mapping a function to a list and then concatenating the list with concat.
-- ghci> concatMap (replicate 4) [1..3]

-- and: takes a list of boolean values and returns True only if all the values in the list are True
-- ghci> and $ map (>4) [5,5,6,7,8]

-- or: is like and only it returns True if any of the boolean values in a list is True.
-- ghci> or $ map (==4) [2,3,4,5,6,1]  

-- any and all:  take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively.
-- ghci> any (==4) [2,3,4,5,6]
-- iterate: takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc.
-- ghci> take 10 $ iterate (*2) 1

-- splitAt: takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple.
-- ghci> splitAt 3 "heyman"

-- takeWhile: It takes elements from a list while the predicate holds and then when an element is encountered that doesn't satisfy the predicate, it's cut off.
-- ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]

-- dropWhile: only it drops all the elements while the predicate is true. Once predicate equates to False, it returns the rest of the list.
-- ghci> dropWhile (/=' ') "This is a sentence"

-- span: is kind of like takeWhile, only it returns a pair of lists
-- The first list contains everything the resulting list from takeWhile would contain if it were called with the same predicate and the same list. The second list contains the part of the list that would have been dropped.
-- ghci> let (fw, rest)  = span (/=' ') "This is a sentence" in "First word: " ++ fw ++ ",  the rest: " ++ rest
-- break: breaks it when the predicate is first true. Doing break p is the equivalent of doing span (not . p). 
-- ghci> break (==4) [1,2,3,4,5,6,7]

-- sort simply sorts a list. The type of the elements in the list has to be part of the Ord typeclass

-- group: takes a list and groups adjacent elements into sublists if they are equal.
-- ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- ghci> map (\l@(x:xs) -> (x, length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

-- inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left.
-- ghci> inits "w00t"
-- ghci> let w = "w00t" in zip (inits w) (tails w)
 
-- Let's use a fold to implement searching a list for a sublist.
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- With the above, we actually just made a function that behaves like isInfixOf  
-- isInfixOf searches for a sublist within a list and returns True if the sublist we're looking for is somewhere inside the target list.
-- ghci> "cat" isInfixOf "im a cat burglar"
-- isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively.
-- elem and notElem check if an element is or isn't inside a list. 

-- partition takes a list and a predicate and returns a pair of lists. The first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't.
-- ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"

-- find takes a list and a predicate and returns the first element that satisfies the predicate. But it returns that element wrapped in a Maybe value.
-- ghci> find (>4) [1,2,3,4,5,6] 

-- elemIndex is kind of like elem, only it doesn't return a boolean value. It maybe returns the index of the element we're looking for. If that element isn't in our list, it returns a Nothing
-- ghci> 4 `elemIndex` [1,2,3,4,5,6]

-- elemIndices is like elemIndex, only it returns a list of indices, in case the element we're looking for crops up in our list several times
-- ghci> ' ' `elemIndices` "Where are the spaces?"

-- findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate
-- findIndices returns the indices of all elements that satisfy the predicate in the form of a list.
-- ghci> findIndex (==4) [5,3,2,1,6,4]
-- ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"

-- zip and zipWith have variants that take more than just two lists i.e zip3, zipWith3 zip4 zipWith4 etc upto 7
-- ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]

-- lines is a useful function when dealing with files or input from somewhere. It takes a string and returns every line of that string in a separate list.
-- ghci> lines "first line\nsecond line\nthird line"
-- unlines is the inverse function of lines. It takes a list of strings and joins them together using a '\n'.
-- ghci unlines ["first line", "second line", "third line"]
-- words and unwords are for splitting a line of text into words or joining a list of words into a text. Very useful.
-- ghci> words "hey these are the words in this sentence"  

-- delete takes an element and a list and deletes the first occurence of that element in the list.
-- ghci> delete 'h' "hey there ghang!"

-- \\ is the list difference function. It acts like a set difference, basically. For every element in the right-hand list, it removes a matching element in the left one.
-- ghci> [1..10] \\ [2,5,9] 

-- union also acts like a function on sets. It returns the union of two lists. It pretty much goes over every element in the second list and appends it to the first one if it isn't already in yet.
-- ghci> [1..7] `union` [5..10]

-- intersect works like set intersection. It returns only the elements that are found in both lists.

-- insert takes an element and a list of elements that can be sorted and inserts it into the last position where it's still less than or equal to the next element.
-- ghci> insert 4 [3,5,1,2,8,2]

-- for the fns we have in the lib there are also variants that are more general in that they take more typeclasses than the 
-- standard ones, for instance length takes Int as one of it's params and returns an Int. 
-- even though they could be more generic and usable if they just took any type that's part of the Integral or Num typeclasses 
-- we have fns such as genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate
-- The nub, delete, union, intersect and group functions all have their more general counterparts called nubBy, deleteBy, unionBy, intersectBy and groupBy.
-- e difference between them is that the first set of functions use == to test for equality, whereas the By ones also take an equality function and then compare them by using that equality function. group is the same as groupBy (==). 

-- An clearer way to write equality functions for the By functions is if you import the on function from Data.Function. on is defined like this: 
-- ghci> groupBy ((=) `on` (> 0)) values



-- some fns in Data.Char

-- The ord and chr functions convert characters to their corresponding numbers and vice versa:
-- The Caesar cipher is a primitive method of encoding messages by shifting each character in them by a fixed number of positions in the alphabet.
encode :: Int -> String -> String 
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted
-- with fn composition above is equivalent to map (chr . (+ shift) . ord) msg

decode :: Int -> String -> String 
decode shift = encode (negate shift)



-- Data.Map funs 
-- some example of when one wants to get the value of a key in a key,value pairing 

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key []  =  Nothing 
findKey key ((k,v):xs) =  if key == k
                          then Just v
                          else findKey key xs

-- We could refactor the above using foldr as we have a classic recursive fn
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing

-- some Data.Map fns
-- The fromList function takes an association list (in the form of a list) and returns a map with the same associations.
-- ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 

-- empty represents an empty map. It takes no arguments, it just returns an empty map.
-- insert takes a key, a value and a map and returns a new map that's just like the old one, only with the key and value inserted.
-- ghci> Map.insert 3 100 Map.empty
-- ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty

-- We can implement our own fromList by using the empty map, insert and a fold
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- null checks if a map is empty.
-- ghci> Map.null $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]

-- size reports the size of a map.
-- ghci> Map.size Map.empty

-- singleton takes a key and a value and creates a map that has exactly one mapping.
-- ghci> Map.singleton 3 9

-- lookup works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key and Nothing if it doesn't.

-- member is a predicate takes a key and a map and reports whether the key is in the map or not.
-- ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]

-- map and filter work much like their list equivalents.
-- ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
-- ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]

-- toList is the inverse of fromList.
-- ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3

-- keys and elems return lists of keys and values respectively
-- keys is the equivalent of map fst . Map.toList and elems is the equivalent of map snd . Map.toList.

phoneBook' =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  
-- now if we just use fromList to put that into a map, we'll lose a few numbers! So here's what we'll do:
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String 
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs  = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 

-- insertWith is to insert what fromListWith is to fromList.
-- It inserts a key-value pair into a map, but if that map already contains the key, it uses the function passed to it to determine what to do.
-- ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]

-- Data.Set: some fn examples
-- The fromList function works much like you would expect. It takes a list and converts it into a set.
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn"
-- ghci> let set1 = Set.fromList text1
-- ghci> let set2 = Set.fromList text2

-- intersection function to see which elements they both share.
-- ghci> Set.intersection set1 set2

--  difference function to see which letters are in the first set but aren't in the second one and vice versa.
-- ghci> Set.difference set1 set2
-- ghci> Set.difference set2 set1

-- we can see all the unique letters used in both sentences by using union.
-- ghci> Set.union set1 set2  

-- The null, size, member, empty, singleton, insert and delete functions all work like you'd expect them to.

-- We can also map over sets and filter them.
-- ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]  
-- ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]  

-- We can implement a less expensive version of the nub op on lists by converting the list into a set first and then converting to list
-- ghci> let setNub xs = Set.toList $ set.fromList xs
-- setNub is generally faster than nub on big lists but as you can see, nub preserves the ordering of the list's elements, while setNub does not


-- THose were examples of importing modules from haskell see Geometry.hs for examples of creating our own modules and exporting them




