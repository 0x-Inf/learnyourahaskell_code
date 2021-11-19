-- solveRPN :: (Num a) => String -> a
-- solveRPN (x:xs) =
--     let opOnlyString = filter isChar xs 
--     -- in head foldl (\acc n -> case n) [] xs
--     where isChar x = x /= ' '

solveRPN' ::  String -> Float
solveRPN' = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs