module AntiPrimes where

import Data.List(find,group)
import Data.Maybe(fromJust)

firstPrimeFactor :: Int -> Int
firstPrimeFactor n = head $ filter ((== 0) . mod n) [2..]

allPrimeFactor :: Int -> [Int]
allPrimeFactor 1 = []
allPrimeFactor n = let first = firstPrimeFactor n
                   in  first : allPrimeFactor ( n `div` first)

factorCount :: Int-> Int
factorCount 1 = 1
factorCount n = product (succ . length <$> group (allPrimeFactor n))

divisorCount :: Int -> (Int, Int)
divisorCount = (,) <*> factorCount

hcnNext :: (Int, Int) -> (Int, Int)
hcnNext (num,factors) = 
    fromJust $ find  ((>factors) . snd) (divisorCount <$> [num ..])

hcnSequence :: [Int]
hcnSequence = fst <$> iterate hcnNext (1,1)

runMain :: IO ()
runMain = print $ take 20 hcnSequence