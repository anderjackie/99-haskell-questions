import System.Random
import Data.List (nub)

rndPermu :: (Eq a) => [a] -> IO [a]
rndPermu l = do
	gen <- getStdGen
	return . take (length l) . nub $ [ l !! x | x <- randomRs ( 0, (length l) - 1 ) gen ]
