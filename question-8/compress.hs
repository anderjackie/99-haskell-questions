compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) =
	let
		rest = [i | i <- xs, i /= x]
	in 
		[x] ++ compress rest
