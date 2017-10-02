--elementAt :: [a] Int -> a
elementAt [] _ = error "No element in an empty list!"
elementAt (x:_) 1 = x
elementAt (x:xs) n
	| n <= 0 = error "index out of bounds"
	| otherwise = elementAt xs (n - 1)
