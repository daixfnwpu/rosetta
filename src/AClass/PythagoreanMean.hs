module AClass.PythagoreanMean where

import Data.List (genericLength)
import Control.Monad (zipWithM)

mean :: Double -> [Double] -> Double
mean  0 xs = product  xs ** (1 / genericLength xs)
mean  p xs = (1 / genericLength xs * sum (map (** p) xs)) ** (1/p)
rootMeanSquare :: [Double] -> Double
rootMeanSquare = sqrt . ((/). foldr ((+) . (^ 2)) 0 <*> genericLength)

runMain :: IO ()
runMain = do
    let ms = zipWith ((. flip mean [1..10]). (,)) "agh" [1,0,-1]
    mapM_ (\(t,m) -> putStrLn $ t : ": " ++ show m) ms
    putStrLn $ " a >= g >= h is " ++ show ((\(_,[a,g,h])-> a>=g && g>=h) (unzip ms))
    print $ rootMeanSquare [1..10]