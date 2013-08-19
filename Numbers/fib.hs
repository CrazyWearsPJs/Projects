{- 
	Fibonacci Sequence - Enter a number and have the program generate
	the Fibonacci sequence to that number or to the Nth number.
-}

import Data.List (genericIndex)

main = 	do
	putStrLn "Enter a number: "
	num <- getLine
	let n = rInteger num
	putStrLn ("Fib(" ++ num ++ ") = " ++ show(getFibonacci $ n))

calcFibonacci :: (Integer -> Integer) -> Integer -> Integer
calcFibonacci p 0 = 1
calcFibonacci p 1 = 1
calcFibonacci p n = p (n - 1) + 
		    p (n - 2)

saveFib :: [Integer]
saveFib =  map (calcFibonacci getFibonacci) [0 .. ]

getFibonacci  :: Integer -> Integer
getFibonacci n = genericIndex saveFib n

rInteger :: String -> Integer
rInteger = read
