module BClass.BinGivenLimits where
import Control.Monad (foldM)
import Data.List (partition)

binSplit :: (Foldable t, Ord a) => t a -> [a] -> [[a]]
binSplit  lims ns = counts ++ [rest]
    where
        (counts,rest) = foldM split ns lims
        split  l i = let (a,b) = partition (< i) l in ( [a],b)

binCounts b = fmap length  . binSplit b

runMain =  putStrLn $ unwords (show <$> binCounts [2,4,7] [1,4,2,6,3,8,9,4,1,2,7,4,1,5,1])