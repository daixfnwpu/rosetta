{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AdditivePrimes where
import Data.List (unfoldr)

primes :: [Int]
primes = 2: sieve [3,5 ..]
    where sieve (x:xs) = x : sieve(filter (\y -> y`mod` x /=0)  xs )
isPrime n = all (\p  -> n `mod` p /= 0 ) $ takeWhile (< sqrtN) primes
    where sqrtN = round . sqrt . fromIntegral $ n

digits = unfoldr f
    where f 0 = Nothing
          f n = let (q,r) = divMod n 10 in Just(r,q)
isAdditivePrime n = isPrime n && (isPrime . sum . digits) n

runMain  =
  --  mapM_ isAdditivePrime [12373,12347,89] 
     mapM_  print  $ takeWhile (< 500) $ filter isAdditivePrime primes