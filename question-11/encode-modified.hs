data TypeValue a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [TypeValue a]
encodeModified [] = []
encodeModified (x:xs) = 
	let
		number = length $ x : takeWhile (==x) xs
		item = 
			if number == 1 then
				Single x
			else
				Multiple (number) x
		rest = dropWhile (==x) xs
	in
		[item] ++ encodeModified rest
