module AClass.AveragesMode where
import Prelude (foldr,maximum,(==),(+), Ord ((>), (<)), map, fst, (.), snd, ($), filter, Eq, Int, otherwise, putStrLn, Show (show), unwords, IO)
import Data.Map (insertWithKey',insertWith',empty,filter,elems,keys, insertWith)
import Data.List(group,sort, head, length, partition)
modeOne :: (Ord a) => [a] -> [a]
modeOne xs = keys (Data.Map.filter (== maximum (elems counts)) counts)
    where counts = foldr (\x -> insertWith (+) x 1) empty xs

modeSecond:: (Ord a ) => [a] -> [a]
modeSecond xs = map fst $ Prelude.filter ((== best).snd) counts
    where counts = map (\l -> (head l , length l)) . group . sort $ xs
          best = maximum (map snd counts)

modeByPartition :: (Eq a) => [a] -> [a]
modeByPartition = snd . modesWithCount
    where modesWithCount :: (Eq a) => [a] ->(Int,[a])
          modesWithCount [] = (0,[])
          modesWithCount l@(x:_) | length xs > best = (length xs,[x])
                                 | length xs < best = (best,modes)
                                 | otherwise        = (best,x:modes)
                where (xs,notxs) = partition (== x) l
                      (best,modes) = modesWithCount notxs

runMain :: IO ()
runMain = do
    let xs = [1,2,3,3,2,1,4] ::[Int]
    putStrLn . unwords . map show  $ modeOne xs
    putStrLn . unwords . map show $ modeSecond xs
    putStrLn . unwords . map show $ modeByPartition xs