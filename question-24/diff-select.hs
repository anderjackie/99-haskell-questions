import System.Random (getStdGen, randomRs)
import Data.List (nub)

diffSelect :: Int -> Int -> IO [Int]
diffSelect n p = do
	gen <- getStdGen
	return . take n . nub $ randomRs (1, p) gen
