data TypeValue a = Single a | Multiple Int a deriving (Show)
decodeModified :: (Eq a) => [TypeValue a] -> [a]
decodeModified [] = []
decodeModified (x:xs) =
	let
		extract :: TypeValue a -> [a]
		extract (Single x) = [x]
		extract (Multiple x a) = replicate x a

		res = extract x
	in
		res ++ decodeModified xs
