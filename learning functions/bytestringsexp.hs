import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- The function pack has the type signature pack :: [Word8] -> ByteString
-- What that means is that it takes a list of bytes of type Word8 and returns a ByteString
-- ghci> B.pack[98..120]

-- unpack is the inverse function of pack. It takes a bytestring and turns it into a list of bytes.

-- fromChunks takes a list of strict bytestrings and converts it to a lazy bytestring. toChunks takes a lazy bytestring and converts it to a list of strict ones.
-- ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  

-- The bytestring version of : is called cons It takes a byte and a bytestring and puts the byte at the beginning.
-- it's lazy so t's better to use the strict version of cons, cons' if you're going to be inserting a lot of bytes at the beginning of a bytestring.
-- ghci> B.cons 85 $ B.pack [80,81,82,84]  
-- ghci> B.cons' 85 $ B.pack [80,81,82,84]  


