insertAt :: (Eq a) => a -> [a] -> Int -> [a]
insertAt i [] _ = [i]
insertAt _ l 0 = l
insertAt i l 1 = i : l
insertAt i (x:xs) n = x : insertAt i xs (n - 1)
