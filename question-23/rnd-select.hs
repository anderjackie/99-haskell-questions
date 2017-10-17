import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect l n = do
	gen <- getStdGen
	return $ take n [ l !! x | x <- randomRs (0, (length l) - 1) gen ]
