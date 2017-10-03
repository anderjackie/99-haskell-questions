encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) =
	let
		n = length $ x : takeWhile (==x) xs
		rest = dropWhile (==x) xs
	in
		[(n, x)] ++ encode rest
