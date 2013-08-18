{-
	Check if Palindrome - Checks if the string entered by the user is a palindrome.
	That is that it reads the same forwards as backwards like “racecar”
-}

main = do
	putStr "Enter string: "
	userString <- getLine
	print  $ userString ++ if isPalindrome userString then " is a palindrome" else " is not a palindrome"

isPalindrome :: String -> Bool
isPalindrome a = a == reverse a