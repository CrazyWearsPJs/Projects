{-
	Pig Latin - Pig Latin is a game of alterations played on the English language game. 
	To create the Pig Latin form of an English word the initial consonant sound is
	transposed to the end of the word and an ay is affixed (Ex.: "banana" would yield anana-bay). 
	Read Wikipedia for more information on rules.
-}

import Data.Char (isAlpha, toLower)
import Data.List
import Data.Ord (comparing)

main = 	do
	userString <- getLine
	putStr "Translated to Pig Latin: "
	print (unwords $ stringToPigLatin $ words $ userString)

stringToPigLatin :: [String] -> [String]
stringToPigLatin a = map wordToPigLatin a

wordToPigLatin :: String -> String
wordToPigLatin a
	| isVowel (head a) =  a ++ "-way" 
	| otherwise = let consonantCluster = getConsonantSound a
		      in (a \\ consonantCluster)  ++ "-" ++ consonantCluster ++ "ay"

isVowel :: Char -> Bool
isVowel a 
	| isAlpha a = toLower(a) `elem` ['a', 'e', 'i', 'o', 'u']
	| otherwise = error "Not a letter."

isConsonant :: Char -> Bool
isConsonant a = not(isVowel a) && toLower(a) /= 'y'

getLettersOnly :: String -> String
getLettersOnly a = takeWhile isAlpha a

getConsonantSound :: String -> String
getConsonantSound a = takeWhile isConsonant a
