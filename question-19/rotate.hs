rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate l 0 = l
rotate l@(x:xs) n
	| n < 0 = rotate l (length l + n)
	| otherwise = rotate (xs ++ [x]) (n - 1)
