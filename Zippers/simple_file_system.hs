
import Data.List (break)
-- Now that we know how zippers work, let's use trees to represent a very simple file system and then make a zipper for that file system,
-- which will allow us to move between folders, just like we usually do when jumping around our file system. 

-- Here's a data type for this and some type synonyms so we know what's what: 
type Name = String
type Data = String 
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = 
    Folder "root"
        [ File "goat_yelling_like_a_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

-- In this case, a breadcrumb should be like a folder, only it should be missing the folder that we currently chose.
-- Why not like a file, you ask? Well, because once we're focusing on a file, we can't move deeper into the file system,
-- so it doesn't make sense to leave a breadcrumb that says that we came from a file. 

-- what should the breadcrumb that we leave look like? 
-- Well, it should contain the name of its parent folder along with the items that come before the file that we're focusing on and the items that come after it. 
-- So all we need is a Name and two lists of items. 

-- Here is the breadCrumb type for the file system 
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

-- and our zipper 
type FSZipper = (FSItem, [FSCrumb])

-- Going back up in the hierarchy is very simple. We just take the latest breadcrumb and assemble a new focus from the current focus and breadcrumb.
-- Like so: 
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- Because our breadcrumb knew what the parent folder's name was,
-- as well as the items that came before our focused item in the folder (that's ls) and the ones that came after (that's rs), moving up was easy. 

-- How about going deeper into the file system? 
-- If we're in the "root" and we want to focus on "dijon_poupon.doc",
-- the breadcrumb that we leave is going to include the name "root" along with the items that precede "dijon_poupon.doc" and the ones that come after it. 

-- Here's a function that, given a name, focuses on a file of folder that's located in the current focused folder: 

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _)     = name == fileName

-- fsTo takes a Name and a FSZipper and returns a new FSZipper that focuses on the file with the given name.
-- That file has to be in the current focused folder.
-- This function doesn't search all over the place, it just looks at the current folder. 

-- First we use break to break the list of items in a folder into those that precede the file that we're searching for and those that come after it.
--  We made an auxilliary function called nameIs that takes a name and a file system item and returns True if the names match. 

-- Now we can move up and down our file system. Let's start at the root and walk to the file "skull_man(scary).bmp": 
-- ghci> let newFocus = (myDisk,[])-: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- ghci> fst newFocus

-- let's move up and then focus on its neighbouring file "watermelon_smash.gif"
-- ghci> let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"
-- ghci> fst newFocus2


-- Manipulating our file system 
-- Now that we know how to navigate our file system, manipulating it is easy. Here's a function that renames the currently focused file or folder: 
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

-- Now we can rename our pics folder
-- ghci> let newFocus = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

-- We descended to the "pics" folder, renamed it and then moved back up
-- How about a function that makes a new item in the current folder? Behold: 

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = 
    (Folder folderName (item:items), bs)

-- Easy as pie. Note that this would crash if we tried to add an item but weren't focusing on a folder, but were focusing on a file instead. 
-- Let's add a file to our "pics" folder and then move back up to the root
-- ghci> let newFocus = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp

-- What's really cool about all this is that when we modify our file system, it doesn't actually modify it in place but it returns a whole new file system. 
-- That way, we have access to our old file system (in this case, myDisk) as well as the new one (the first component of newFocus).
-- So by using zippers, we get versioning for free, meaning that we can always refer to older versions of data structures even after we've changed them, so to speak. 
