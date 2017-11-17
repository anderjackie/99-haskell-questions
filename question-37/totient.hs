import Data.List(group)

coprime :: Int -> Int -> Bool
coprime n m = (gcd n m) == 1

totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (\x -> coprime n x) [1..(n - 1)]

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

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = 
	let
		pf = map head $ group $ primeFactors n
		mult = map length $ group $ primeFactors n
	in
		zip pf mult

totientIM :: Int -> Int
totientIM n = product . map (\(p, m) -> (p - 1) * p ^ (m - 1)) $ prime_factors_mult n
