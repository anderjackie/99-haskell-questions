isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n 
	| n <= 0 = False
	| even n = False
	| otherwise = and $ map (\x -> (n `rem` x) /= 0) [3..(n - 1)] 

primesR :: Int -> Int -> [Int]
primesR n m = [i | i <- [n..m], isPrime i]
