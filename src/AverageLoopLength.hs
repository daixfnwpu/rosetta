{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module AverageLoopLength where
import System.Random (mkStdGen, Random (randomR), RandomGen)
import Text.Printf (printf, PrintfArg)
import qualified Data.Set as S

runMain :: IO ()
runMain = do
    putStrLn " N     average     analytical        (error)"
    putStrLn "===    =======      =========        ======="
    let samples = 10000 :: Integer
        range   = [1..20] ::[Integer]
    test samples range $ mkStdGen 0
    return ()
test ::(Integral t1,Random a ,Integral a,RandomGen t2,Text.Printf.PrintfArg a)
  => t1 -> [a] -> t2 -> IO t2
test _   [] gen = return gen
test  samples (x:xs) gen =  do
    let (st,gen') = statistical samples x gen
        an        = analytical x
        err       = abs (st -an) / st * 100.0
        str       = printf "%3d %9.4f   %12.4f   (%6.2f%%)\n"
                    x (st :: Float) (an :: Float) (err:: Float)
    putStr str
    test samples xs gen'

analytical ::(Integral a, Fractional b) =>  a -> b
analytical n =  sum [fromIntegral (factorial n) /
                     fromIntegral (factorial (n-i))/
                     fromIntegral (n ^ i) |
                     i <- [1..n]]

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

statistical :: ( Integral t, RandomGen t1,Integral  a,Random a, Fractional a2) => t -> a -> t1 -> (a2, t1)
statistical samples size gen=
    let (total,gen') = sar samples gen 0
    in (fromIntegral  total / fromIntegral samples,gen')
    where
        sar 0  gen' acc = (acc,gen')
        sar samples' gen' acc =
            let (len,gen'') = findRep size gen'
            in sar (samples' - 1) gen'' (acc + len)
findRep :: (Random a,Integral a,RandomGen b) => a -> b -> (a, b)
findRep n = findReg' (S.singleton 1) 1
    where findReg' seen len gen'
           | S.member fx seen = (len,gen'')
           | otherwise        = findReg' (S.insert fx seen) (len + 1) gen''
                where
                    (fx,gen'') = randomR  (1,n) gen'
