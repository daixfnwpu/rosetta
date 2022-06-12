{-# LANGUAGE TupleSections #-}
module DerangedAnagrams where

import Data.List (maximumBy,sort,unfoldr)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S

groupBySig:: [String] -> [(String,S.Set String)]
groupBySig = map ((,) . sort <*> S.singleton )

equivs :: [(String,S.Set String)] -> [[String]]
equivs = map (S.toList . snd) . M.toList . M.fromListWith S.union 

isDerangeMent ::(String,String) -> Bool
isDerangeMent (a,b) = and $ zipWith (/=)  a b

pairs :: [t] -> [(t,t)]
pairs = concat . unfoldr  step
    where
        step (x:xs) = Just (map (x,) xs,xs)
        step [] = Nothing
anagrams :: [String] -> [(String,String)]
anagrams = concatMap pairs . equivs . groupBySig

maxDerangedAnagram :: [String] -> Maybe (String, String)
maxDerangedAnagram = maxByLen . filter isDerangeMent  . anagrams
    where maxByLen [] = Nothing
          maxByLen xs = Just $ maximumBy (comparing (length . fst)) xs

runMain :: IO ()
runMain = do
    input <- readFile "data/unixdict.txt"
    case maxDerangedAnagram $ words  input of
        Nothing -> putStrLn "No deranged anagrams were found"
        Just (a,b) -> putStrLn $ "Longest deranged anagrams: " <> a <> " and " <> b
