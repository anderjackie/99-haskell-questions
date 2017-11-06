combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n l@(x:xs)
	| n >= (length l) = [l]
	| otherwise = [ x : i | i <- combinations (n - 1) xs ] ++ combinations n xs
