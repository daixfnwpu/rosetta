{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AClass.FourRingsSquaresPuzzle where
import Data.List
import Control.Monad

perms [] = [[]]
perms xs = [ x:xr | x <- xs,xr <- perms (xs \\ [x])]

combs 0 [] = [[]]
combs n xs = [ x:xr | x <- xs,xr <- combs (n-1) xs]

ringCheck :: [Int] -> Bool
ringCheck [x0,x1,x2,x3,x4,x5,x6] = 
        v == x1 + x2 + x3
     && v == x3 + x4 + x5
     && v == x5 + x6
    where v = x0 + x1

fourRings low high allowRepeats verbose = do
    let candidates = if allowRepeats 
                        then combs 7 [low .. high]
                        else perms [low..high]
        solutions = filter ringCheck candidates
    when verbose $ mapM_ print solutions
    putStrLn $ show (length solutions)
                ++ (if allowRepeats then " non" else "")
                ++ " unique solutions for "
                ++ show low
                ++ " to"
                ++ show high
    putStrLn ""
runMain = do
    fourRings 1 7 False True
    fourRings 3 9 False True
    fourRings 0 9 True  False 