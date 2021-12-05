main = interact respondPalindromes


respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "Palindrome" else "not a palindrome") . lines 
    where  isPalindrome xs = xs == reverse xs

