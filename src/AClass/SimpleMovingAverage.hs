{-# LANGUAGE BangPatterns #-}
module AClass.SimpleMovingAverage where

import Control.Monad
import Data.IORef
import Data.List
import Control.Arrow
import Control.Monad.State
import Data.Functor
import Control.Applicative (liftA3)
import AClass.Abelian (runM, runMain)
import Text.Printf (printf)

data Pair a b = Pair !a !b

mean :: Fractional a => [a] -> a
mean = divl . foldl' (\(Pair s l) x -> Pair (s+x) (l+1)) (Pair 0.0 0)
    where divl (Pair _ 0) = 0.0
          divl (Pair s l) = s / fromInteger l
series :: [Double]
series = [1,2,3,4,5,5,4,3,2,1]

mkSMA :: Int -> IO (Double->IO Double)
mkSMA period = avgr <$> newIORef []
    where avgr nsref x = readIORef nsref >>= (\ns ->
                    let xs = take period (x:ns)
                    in writeIORef nsref xs $> mean xs)
runSMA :: IO [String]
runSMA = mkSMA 3 >>= (\sma3 -> mkSMA 5 >>= (\sma5-> mapM (\n -> str n <$> sma3 n <*> sma5 n ) series))
   where str n mm3 mm5 = concat ["Next number = ", show n,", SMA_3=",show mm3 ,",SMA_5 = ",show mm5]

runMain :: IO ()
--runMain = runSMA >>= print
runMain = printSMA 3 5

sMA :: Fractional a => Int -> [a] -> [(a, a)]
sMA p = map (head *** head) .tail .
        scanl (\(y,_) -> (id &&& return . av) . (: if length y==p then init y else y)) ([],[])
    where av = liftM2 (/) sum (fromIntegral .length)
printSMA :: Int -> Int -> IO ()
printSMA n p = mapM_ (\(n,a) -> putStrLn $ "Next number: " ++ show n ++ " Average: " ++ show a) .take n . sMA p $ [1..5] ++ [5.4..1] ++ [3..]
