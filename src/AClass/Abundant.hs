module AClass.Abundant where

import Data.List(nub)

divisorSum :: Integral a => a -> a
divisorSum n = 
    sum . map (\i -> sum $ nub [i,n `quot` i])
    . filter ((==0) . (n `rem`)) 
    $ takeWhile ((<=n) . (^ 2)) [1 ..]

oddAbundants :: Integral a => a -> [(a,a)]
oddAbundants n = 
    [(i,divisorSum i) | i<- [n..],odd  i,divisorSum i > i*2]

printAbundant :: (Show a1, Show a2) => (a1, a2) -> IO ()
printAbundant (n,s) =
    putStrLn $ show n  ++ " with " ++ show s ++ " as the sum of all proper divisors"

classOf :: Integral a => a -> Ordering
classOf n = compare (sum $ filter ((==0) . (n `mod`)) [ 1.. (n`div` 2)]) n 

runMain :: IO ()
runMain = do
    putStrLn " The first 25 odd abundant numbers are :"
    mapM_ printAbundant . take 25 $ oddAbundants 1
    putStrLn " The 1000th odd abundant number is:"
    printAbundant $ oddAbundants 1 !! 1000
    putStrLn "The first odd abundant number above 100000000 is:"
    printAbundant . head . oddAbundants $ 10 ^ 9
    let classes = map classOf [1 .. 20000 ::Int]
        printRes w c = putStrLn $ w ++ (show . length $ filter (==c) classes)
    printRes "deficient: " LT
    printRes "perfect: "   EQ
    printRes "abundant: "  GT
