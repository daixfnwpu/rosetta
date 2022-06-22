{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AdditionChainsExponentiation where

binaryChain :: Integral a => a -> [a]
binaryChain 1 = [1]
binaryChain n | even n = n : binaryChain (n `div` 2)
              | odd  n = n : binaryChain (n - 1)