{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NineBillonNamesGod where
import Data.List

rows = snd $ mapAccumL  f [] cumu where--(a -> b -> (a, c)) a (t b)
    f r row = (rr,new_row) where
        new_row = map head rr
        rr = map tailKeepOne (row:r)
    tailKeepOne [x] = [x]
    tailKeepOne (_:xs) = xs
    cumu = [1] : map (scanl (+) 0) rows
runMain = mapM_ print $ take 10 rows--(a -> m b) (t a)