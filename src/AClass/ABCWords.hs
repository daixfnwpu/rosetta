module AClass.ABCWords where
import Data.Maybe (isJust, catMaybes, mapMaybe)

afterChar c except = go
    where
        go [] = Nothing
        go (x:xs)
            | x `elem` except = Nothing
            | x == c          = Just xs
            | otherwise       = go xs

isABC :: String -> Bool 
isABC s = isJust $ afterChar 'a' "bc" s >>= afterChar 'b' "c" >>= afterChar 'c' ""

runMain = do
    readFile  "data/unixdict.txt" >>= mapM_ print . zip [1 ..] . filter  isABC . lines