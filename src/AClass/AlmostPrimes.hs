module AClass.AlmostPrimes where
import Data.List
import qualified Control.Monad
isPrime :: Integer -> Bool
isPrime n = not $ any ((== 0) . mod n ) [2 .. (truncate . sqrt $ fromInteger n )]

primes :: [Integer]
primes = filter isPrime [2..]

isKPrime :: (Eq t, Num t) => t -> Integer -> Bool
isKPrime 1 n = isPrime n
isKPrime k n = any (isKPrime (k-1)) sprimes
    where sprimes = map fst $ filter ((== 0) . snd) $ map (divMod n) $ takeWhile (< n) primes

kPrimes :: (Eq t, Num t) => t -> [Integer]
kPrimes k = filter (isKPrime k) [2..]

runMain :: IO()
runMain = Control.Monad.forM_  [1..5] $ \k ->
    putStrLn $ "k=" ++ show k  ++ ": " ++ unwords (map show (take 10 $ kPrimes k))