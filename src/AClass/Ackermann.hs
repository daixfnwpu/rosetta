module AClass.Ackermann where

import Data.List (mapAccumL)

ack 0 n = succ n
ack m 0 = ack (pred m) 1
ack m n = ack (pred m) (ack m (pred n))


ackermann =  iterate ackfun [1..] where
    ackfun a = s where
        s = snd $ mapAccumL  f (tail a) (1 : zipWith (-)  s  (1:s))
        f a b = (aa,head aa) where aa = drop b a

runMain = mapM_ (print . (\n -> take (6 - n) $ ackermann !! n)) [0..5]