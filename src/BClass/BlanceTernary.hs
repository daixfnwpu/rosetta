{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BClass.BlanceTernary
where

data BalancedTenary = Bt [Int]

zeroTrim a = if null s then [0] else s where
    s = fst $ foldl f ([],[]) a
    f (x,y) 0 = (x, y ++ [0])
    f (x,y) z = (x ++ y ++ [z],[])
btList (Bt a) = a

instance Eq BalancedTenary where
    (==) a b = btList a == btList b
listBt = Bt . zeroTrim
btNormalize = listBt . _carray 0 where
    _carray c [] = [c | c /= 0]
    _carray c (a:as) = r : _carray cc as where
        (cc,r) = f $ (a+c) `quotRem` 3 where
            f (x,2) = (x+1,-1)
            f (x,-2) = (x-1,1)
            f x = x

instance Show BalancedTenary where
    show = reverse .map (\case  -1 -> '-'; 0 -> '0'; 1 -> '+') . btList

strBt = Bt . zeroTrim . reverse . map (\case '-' -> -1; '0' -> 0; '+' ->1) 

intBt :: Integral a => a -> BalancedTenary
intBt = fromIntegral . toInteger

listAdd a b = take (max (length a) (length b)) $ zipWith (+) (a ++ [0..0]) (b ++ [0..0])
instance Num BalancedTenary where
    negate = Bt . map negate . btList
    (+) x y =btNormalize $ listAdd (btList x) (btList y)
    (*) x y =btNormalize $ mul_ (btList x) (btList y) where
        mul_ _ [] = []
        mul_ as b = foldr (\a z -> listAdd (map (a*) b) (0:z)) [] as
