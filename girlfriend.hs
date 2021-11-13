import System.IO
import Data.Char

{-|
main = do
    handle <- openFile "girlfriend.txt" ReadMode 
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
-}

-- Another way of doing what we just did is to use the withFile function,
-- which has a type signature of withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- It takes a path to a file, an IOMode and then it takes a function that takes a handle and returns some I/O action

{-
main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-}

-- For better understanding of what withFile is doing, here is our own implementation
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-- Just like we have hGetContents that works like getContents but for a specific file, there's also hGetLine, hPutStr, hPutStrLn, hGetChar, etc
-- They work just like their counterparts without the h, only they take a handle as a parameter and operate on that specific file instead of operating on standard input or standard output. 

-- readFile has a type signature of readFile :: FilePath -> IO String
-- Here's how we could have written our previous example with readFile:
{-
main = do 
    contents <- readFile "girlfriend.txt"
    putStr contents
-}

-- writeFile has a type of writeFile :: FilePath -> String -> IO ()
-- It takes a path to a file and a string to write to that file and returns an I/O action that will do the writing.
-- If such a file already exists, it will be stomped down to zero length before being written on.
-- Here's how to turn girlfriend.txt into a CAPSLOCKED version and write it to girlfriendcaps.txt: 
{-
main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriend.txt" (map toUpper contents)
-}

-- appendFile has a type signature that's just like writeFile, only appendFile doesn't truncate the file to zero length if it already exists but it appends stuff to it.

-- Sometimes you want to control how much of a file you want to read i.e size of the chunks
-- You can control how exactly buffering is done by using the hSetBuffering function.
-- It takes a handle and a BufferMode and returns an I/O action that sets the buffering.
-- BufferMode is a simple enumeration data type and the possible values it can hold are: NoBuffering, LineBuffering or BlockBuffering (Maybe Int)
-- Here is our example(not writing) but reads the whole file in chunks of 2048 bytes.
main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
-- Reading files in bigger chunks can help if we want to minimize disk access or when our file is actually a slow network resource.
-- We can also use hFlush, which is a function that takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle.


