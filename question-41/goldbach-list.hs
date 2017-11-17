isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n 
	| n <= 0 = False
	| even n = False
	| otherwise = and $ map (\x -> (n `rem` x) /= 0) [3..(n - 1)] 

goldbach :: Int -> (Int,Int)
goldbach n
	| not (even n) || n <= 2 = error "number must be even and bigger than 2"
	| otherwise = head [(i, j) | i <- [2..n], j <- [2..n], i + j == n, isPrime i, isPrime j]

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList n m = [goldbach k | k <- filter even [n..m], k > 2]

goldbachList' :: Int -> Int -> [(Int,Int)]
goldbachList' n m = filter (\(x, y) -> x > 50 && y > 50) $ goldbachList n m
