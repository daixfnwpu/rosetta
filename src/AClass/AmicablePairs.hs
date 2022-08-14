module AClass.AmicablePairs where
import AClass.AliquotSeq (divisors)
import Data.List (nub)

runMain :: IO ()
runMain = do 
    let range = [1.. 20000::Int]
        divs  = zip range $ map (sum . divisors) range
        pairs = [(n,m) | (n,nd) <- divs,(m,md)<-divs,n<m,nd==m,md==n]
   -- print . nub $ divs
    print pairs