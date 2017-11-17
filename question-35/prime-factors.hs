isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n 
	| n <= 0 = False
	| even n = False
	| otherwise = and $ map (\x -> (n `rem` x) /= 0) [3..(n - 1)] 

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = x : primeFactors (quot n x)
	where 
		x = head [a | a <- [2..n], isPrime a, n `rem` a == 0] 
