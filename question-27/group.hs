import qualified Data.List as L

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n l@(x:xs)
	| n >= (length l) = [l]
	| otherwise = [ x : i | i <- combinations (n - 1) xs ] ++ combinations n xs

group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group [] l = [[[]]]
group n [] = [[[]]]
group [n] l =[combinations n l]
group (n:ns) l = [i : j | i <- combinations n l, j <- group ns (l L.\\ i)]
