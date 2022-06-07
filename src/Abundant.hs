module Abundant where

import Data.List(nub)

divisorSum n = 
    sum . map (\i -> sum $ nub [i,n `quot` i])
    . filter ((==0) . (n `rem`)) 
    $ takeWhile ((<=n) . (^ 2)) [1 ..]

oddAbundants :: Integral a => a -> [(a,a)]
oddAbundants n = 
    [(i,divisorSum i) | i<- [n..],odd  i,divisorSum i > i*2]

printAbundant (n,s) =
    putStrLn $ show n  ++ " with " ++ show s ++ " as the sum of all proper divisors"

runMain = do
    putStrLn " The first 25 odd abundant numbers are :"
    mapM_ printAbundant . take 25 $ oddAbundants 1
    putStrLn " The 1000th odd abundant number is:"
    printAbundant $ oddAbundants 1 !! 1000
    putStrLn "The first odd abundant number above 100000000 is:"
    printAbundant . head . oddAbundants $ 10 ^ 9
