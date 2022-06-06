module ABCProblem where

import Data.List (delete)
import Data.Char (toUpper)

abc ::(Eq a) => [[a]] ->[a] ->[[[a]]]
abc _ [] = [[]]
abc blocks (c:cs) = [b:acs | b <-blocks , c `elem` b,acs <- abc (delete b blocks) cs ] 


blocks = ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
          "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"]


runMain = do
    mapM_ (\w -> print (w,not . null $ abc blocks w) ) 
        ["", "A", "BARK", "BoOK", "TrEAT", "COmMoN", "SQUAD", "conFUsE"]