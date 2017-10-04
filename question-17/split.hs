split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split l n = (firstList l n, rest l n)
	where
		firstList :: [a] -> Int -> [a]
		firstList [] _ = []
		firstList l 0  = []
		firstList (x:xs) n = x : firstList xs (n - 1)

		rest :: [a] -> Int -> [a]
		rest [] n = []
		rest l 0 = l
		rest (x:xs) n = rest xs (n - 1)

{-
split :: [a] -> Int -> ([a], [a])
split []         _             = ([], [])
split l@(x : xs) n | n > 0     = (x : ys, zs)
                   | otherwise = ([], l)
    where (ys,zs) = split xs (n - 1)
-}
