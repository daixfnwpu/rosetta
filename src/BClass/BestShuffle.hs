module BClass.BestShuffle where
import Data.Vector((//),(!))
import qualified Data.Vector as V
import Data.List (delete,find)

swapShuffle :: Eq a => [a] -> [a] -> [a]
swapShuffle lref lst = V.toList $ foldr adjust (V.fromList lst) [0 .. n-1]
    where
        vref = V.fromList lref
        n = V.length vref
        adjust i v = case find alternative [ 0 .. n-1] of
            Nothing -> v
            Just j  -> v // [(j,v!i),(i,v!j)]
            where
                alternative j = and [v!i == vref!i
                                    ,i /= j
                                    ,v!i /= vref!j
                                    ,v!j /= vref!i]

shuffle :: Eq a => [a] -> [a]
shuffle lst = swapShuffle lst lst

shufflingQuality :: Eq b => [b] -> [b] -> Int
shufflingQuality l1 l2 = length $ filter id $ zipWith (==) l1 l2

printTest :: ([Char] -> IO [Char]) -> IO ()
printTest prog = mapM_ test texts
    where
        test s = do
                x <- prog s
                putStrLn $ unwords [ show s
                                    , show x
                                    , show $ shufflingQuality s x]
        texts = [ "abba", "abracadabra", "seesaw", "elk" , "grrrrrr"
                , "up", "a", "aaaaa.....bbbbb"
                , "Rosetta Code is a programming chrestomathy site." ]
runMain = printTest (pure . shuffle)