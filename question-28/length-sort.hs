import Data.List
import Data.Function

lsort :: [[a]] -> [[a]]
lsort [[]] = [[]]
lsort [l] = [l]
lsort l = sortBy (compare `on` length) l

-- lfsort ["a", "ab", "ce", "abcd"] = ["a", "abcd", "ab", "ce"]
--                                      1      1    |---2----| 
-- groupBy ((==) `on` length)
lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort
