gcd' :: Int -> Int -> Int
gcd' 0 _ = 0
gcd' _ 0 = 0
gcd' m n
	| m == n = m
	| m < n = gcd' m (n - m)
	| otherwise = gcd' (m - n) n 
