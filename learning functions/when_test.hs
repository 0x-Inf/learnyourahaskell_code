import Control.Monad

-- Here's how we could rewrite the piece of code with which we demonstrated getChar by using when:

main = do 
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main