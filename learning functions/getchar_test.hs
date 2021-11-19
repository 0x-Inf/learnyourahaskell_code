-- getChar is an I/O action that reads a character from the input
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()