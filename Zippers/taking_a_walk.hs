-- In this Zipper chapter we're going to see how we can take a data structure and only focus on a part of it in a way that makes changing its elements 
-- easy and walking around it efficient 

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- Here is an example of such a Tree
freeTree :: Tree Char 
freeTree = 
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- What if we wanted to change the W to a P
-- We could pattern match on our tree until we find the element 
-- Here is code for that 
changeToP' :: Tree Char -> Tree Char 
changeToP' (Node x l (Node y (Node _ m n) r )) = Node x l (Node y (Node 'P' m n) r)

-- The above is not pleasing to look at
-- Is there a better way to do this?
-- How about a fn that takes a tree along with a list of directions

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Tree Char -> Directions -> Tree Char 
changeToP (Node x l r) (L:ds) = Node x (changeToP l ds) r
changeToP (Node x l r) (R:ds) = Node x l (changeToP r ds)
changeToP (Node _ l r) []     = Node 'P' l r

-- To avoid printing out the whole tree we're going to make a fn that prints out the element at the node which a set of directions brings us to
elemAt :: Directions -> Tree a -> a 
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _)     = x

-- In these functions, the list of directions acts as a sort of focus, because it pinpoints one exact sub-tree from our tree. 

-- While this technique might seem cool, it's rather inefficient, think about how we'll repeatedly change items
-- and some can be on the bottom of a huge tree 


-- Would it help if we start at the root of the tree and move either left or right one step at a time and sort of leave breadcrumbs?
-- i.e when we go left, we remember that we went left and when we go right, we remember that we went right. Sure, we can try that. 

--  because our directions will now be reversed since we're leaving them as we go down our tree: 
type Breadcrumbs' = [Direction]

-- Here's a function that takes a tree and some breadcrumbs and moves to the left sub-tree while adding L to the head of the list that represents our breadcrumbs: 
goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L:bs)

-- And to go right 
goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R:bs)

-- Let's use these functions to take our freeTree and go right and then left: 
-- ghci> goLeft (goRight (freeTree, []))

-- We can use the -: to make the fn application more clearer. This is the signature x -: f = f x
-- ghci> (freeTree, []) -: goRight -: goLeft

-- Going back up 
-- What if we now want to go back up in our tree? Our Breadcrumbs strategy does not let us do that 
-- In general, a single breadcrumb should contain all the data needed to reconstruct the parent node. 
-- So it should have the information from all the paths that we didn't take and it should also know the direction that we did take, 
-- but it must not contain the sub-tree that we're currently focusing on.

-- Let's modify our breadcrumbs so that they also contain information about everything that we previously ignored when moving left and right.
-- Instead of Direction, we'll make a new data type: 
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

-- Now, instead of just L, we have a LeftCrumb that also contains the element in the node that we moved from and the right tree that we didn't visit.
-- Instead of R, we have RightCrumb, which contains the element in the node that we moved from and the left tree that we didn't visit. 

-- We have a new type synonym
type Breadcrumbs a = [Crumb a]

-- Next up, we have to modify the goLeft and goRight functions to store information about the paths that we didn't take in our breadcrumbs, 
-- instead of ignoring that information like they did before 

goLeft' :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r:bs)


goRight' :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight' (Node x l r, bs) = (r, RightCrumb x l:bs)

-- We were previously able to go left and right. 
-- What we've gotten now is the ability to actualy go back up by remembering stuff about the parent nodes and the paths that we didn't visit.
goUp' :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp' (t, LeftCrumb x r:bs)  = (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = (Node x l t, bs)

-- With a pair of Tree a and Breadcrumbs a, we have all the information to rebuild the whole tree and we also have a focus on a sub-tree. 
-- Such a pair that contains a focused part of a data structure and its surroundings is called a zipper,
-- because moving our focus up and down the data structure resembles the operation of a zipper on a regular pair of pants.

type Zipper a = (Tree a, Breadcrumbs a)

-- Manipulating trees under focus
-- Now that we can move up and down, let's make a function that modifies the element in the root of the sub-tree that the zipper is focusing on: 
modify :: (a -> a) -> Zipper a -> Zipper a 
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs)      = (Empty, bs)

-- example 
-- ghci> let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree, [])))
-- ghic> let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')

-- We can then move up if we want and replace an element with a mysterious 'X': 
-- ghci> let newFocus2 = modify (\_ -> 'X') (goUp newFocus)
-- or 
-- ghci> let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

-- Each node has two sub-trees, even if those sub-trees are empty trees. 
-- So if we're focusing on an empty sub-tree, one thing we can do is to replace it with a non-empty subtree, thus attaching a tree to a leaf node
attach :: Tree a -> Zipper a -> Zipper a 
attach t (_, bs) = (t,bs)

-- We take a tree and a zipper and return a new zipper that has its focus replaced with the supplied tree. 
-- Not only can we extend trees this way by replacing empty sub-trees with new trees, we can also replace whole existing sub-trees.
-- Let's attach a tree to the far left of our freeTree: 
-- ghci> let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft
-- ghic> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)

-- newFocus is now focused on the tree that we just attached and the rest of the tree lies inverted in the breadcrumbs.
-- If we were to use goUp to walk all the way to the top of the tree, it would be the same tree as freeTree but with an additional 'Z' on its far left. 

-- Making a function that walks all the way to the top of the tree, regardless of what we're focusing on, is really easy. Here it is: 
topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z      = topMost (goUp z)

-- If our trail of beefed up breadcrumbs is empty, this means that we're already at the root of our tree, so we just return the current focus.
-- Otherwise, we go up to get the focus of the parent node and then recursively apply topMost to that. 
-- So now we can walk around our tree, going left and right and up, applying modify and attach as we go along
-- and then when we're done with our modifications, we use topMost to focus on the root of our tree and 
-- see the changes that we've done in proper perspective. 

-- We now use the Maybe Monad to handle for failures 
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _)       = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _)       = Nothing 

-- Cool, now if we try to take a step to the left of an empty tree, we get a Nothing! 
-- ghci> goLeft (Empty, [])
-- ghci> goLeft (Node 'A' Empty Empty, [])

-- Looks good! How about going up? The problem before happened if we tried to go up but we didn't have any more breadcrumbs,
-- which meant that we were already in the root of the tree.

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs)  = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, [])                = Nothing 

-- If we have breadcrumbs, everything is okay and we return a successful new focus, but if we don't, then we return a failure. 
-- ghci> let coolTree = Node 1 Empty (Node 3 Empty Empty)
-- ghci> return (coolTree,[]) >>= goRight
-- ghci> return (coolTree,[]) >>= goRight >>= goRight
-- ghci> return (coolTree,[]) >>= goRight >>= goRight >>= goRight



-- Focusing on Lists
-- Zippers can be used with pretty much any data structure, so it's no surprise that they can be used to focus on sub-lists of lists. 

-- Because a single breadcrumb here is just the element, we don't really have to put it inside a data type, like we did when we made the Crumb data type for tree zippers: 
type ListZipper a = ([a],[a])

-- The first list represents the list that we're focusing on and the second list is the list of breadcrumbs. Let's make functions that go forward and back into lists: 
goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- When we're going forward, we focus on the tail of the current list and leave the head element as a breadcrumb.
-- When we're moving backwards, we take the latest breadcrumb and put it at the beginning of the list. 

-- Some examples
-- ghci> let xs = [1,2,3,4]
-- ghci> goForward (xs,[])
-- ghci> goForward ([2,3,4], [1])
-- ghci> goForward ([3,4], [2,1])
-- ghic> goBack ([4], [3,2,1])


