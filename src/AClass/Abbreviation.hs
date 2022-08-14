module AClass.Abbreviation where
import Data.List (inits,transpose,intercalate)
import qualified Data.Set  as S

minAbbrevnLength [] = 0
minAbbrevnLength xs = 
    length . head. S.toList . head $ 
        dropWhile ((<n) . S.size ) $
            S.fromList <$> transpose (inits <$> xs)
    where
        n = length xs 

runMain = do
    s <-readFile "data/weekDayNames.txt"
    mapM_ putStrLn $
        take 10 $
            intercalate "\t" 
                . (<*>) [show . minAbbrevnLength . words,id] .return <$> lines  s


