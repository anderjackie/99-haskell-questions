removeAt :: (Eq a) => Int -> [a] -> (a, [a]) 
removeAt n l = (i, [j | j <- l, j /= i])
	where
		i | n > 0 = l !! (n - 1) | otherwise = error "not a valid index"
