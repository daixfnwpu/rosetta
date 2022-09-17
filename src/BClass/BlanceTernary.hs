{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module BClass.BlanceTernary
where

data BalancedTenary = Bt [Int]

zeroTrim a = if null s then [0] else s where
    s = fst $ foldl f ([],[]) a 
    f (x,y) 0 = (x, y ++ [0])
    f (x,y) z = (x ++ y ++ [z],[])
