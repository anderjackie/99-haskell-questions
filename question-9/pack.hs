pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) =
	let
		packed = x : takeWhile (==x) xs
		rest = dropWhile (==x) xs
	in
		[packed] ++ pack rest
