dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery l 0 = l
dropEvery l n = list ++ dropEvery rest n
	where
		(list, rest) = (init (take n l), drop n l)
