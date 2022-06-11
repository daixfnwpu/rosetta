module AlmkvistGiullera where

import Control.Monad
import Data.Number.CReal
import GHC.Integer
import Text.Printf
interations :: Integer
interations = 52
runMain :: IO()
runMain = do
    printf "N. %44s %4s %s\n" 
            "Integral part of Nth term" "x10^" "=Actual value of Nth term"
    forM_ [0..9]  $ \n -> 
        printf "%d. %44d %4d %s\n" n 
                                (almkvistGiulleraIntegral n)
                                (tenExponent n)
                                (showCReal 50 (almkvistGiullera n))
    printf "\n Pi after %d interations :\n" interations
    putStrLn $ showCReal  70 $ almkvistGiulleraPi interations

almkvistGiulleraIntegral n = 
    let polynomial = (532 `timesInteger` n `timesInteger` n) `plusInteger` (126 `timesInteger` n) `plusInteger` 9
        numerator  = 32 `timesInteger` facInteger (6 `timesInteger` n) `timesInteger ` polynomial
        denominator = 3 `timesInteger` powInteger (facInteger n) 6 
    in numerator `divInteger` denominator
tenExponent n = 3 `minusInteger` (6 `timesInteger` (1 `plusInteger` n))
almkvistGiullera n = fromInteger (almkvistGiulleraIntegral n) / fromInteger (powInteger 10 (abs $ tenExponent n))
almkvistGiulleraSum n = sum $ map almkvistGiullera [0..n]
almkvistGiulleraPi  n = sqrt $ 1 / almkvistGiulleraSum n
facInteger n = if n `leInteger` 1 then 1 else n `timesInteger` facInteger (n `minusInteger` 1)

powInteger 1 _ = 1
powInteger _ 0 = 1
powInteger b 1 = b
powInteger b e = b `timesInteger` powInteger b (e `minusInteger` 1)