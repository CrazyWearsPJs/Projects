{-
	Count Words in a String -  Counts the number of individual words in a string. 
	For added complexity read these strings in from a text file and generate a summary.
-}

import Data.Char (toLower)
import Data.List (sortBy, sort, group, nub)
import Data.Ord (comparing)

main = do
	putStr "Enter string: "
	userString <- getLine
	putStr "Top 10 words:"
	print (getTopWords (getFrequencyPairs.words.lowercase $ userString)  10)


lowercase :: [Char] -> [Char]
lowercase a = map toLower a

getTopWords :: Ord b => [(a, b)] -> Int -> [(a, b)]
getTopWords list n = take n $ reverse $ sortBy(comparing snd) list

getFrequencyPairs :: Ord a => [a] -> [(a, Int)]
getFrequencyPairs list =   let sortedWords    = sort $ list
                               frequencyList = map (\a -> length a).group $ sortedWords
							in zip (nub $ sortedWords) frequencyList