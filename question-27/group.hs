combination :: [Int] -> [String] -> [[String]]
combination [n] l = [take n l]
combination (n:ns) l = [take n l] ++ combination ns (drop n l)

group :: [Int] -> [String] -> [[String]]
group n l = [ (take i l) : j | i <- n, j <- (group n (drop i l)) ]
