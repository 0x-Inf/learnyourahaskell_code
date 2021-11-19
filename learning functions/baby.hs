doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'brien = "It's a-me, Conan O'Brien!"

triangles as bs cs = [(a,b,c) | c <- cs, b <- bs, a <- cs]

-- let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b]]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st , c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int 
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n  = product [1..n]

-- Float is a real floating point with single precision
circumference :: Float -> Float
circumference r  = 2 * pi * r

-- Double is a real floating point with double  precision
circumference' :: Double -> Double 
circumference' r = 2 * pi * r

