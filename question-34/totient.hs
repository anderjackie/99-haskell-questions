coprime :: Int -> Int -> Bool
coprime n m = (gcd n m) == 1

totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (\x -> coprime n x) [1..(n - 1)]
