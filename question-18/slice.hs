slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice l@(x:xs) i k 
	| k < i  = []
	| i > 1 = slice xs (i - 1) (k - 1)
	| k > 0 = x : slice xs i (k - 1)
	| otherwise = l
