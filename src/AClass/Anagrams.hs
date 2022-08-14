module AClass.Anagrams where

import Data.List ( groupBy, sort )
groupon :: Eq a => (t -> a) -> t -> t -> Bool
groupon f x y =  f x == f y

runMain :: IO ()
runMain = do
    f <- readFile "data/unixdict.txt"
    let words = lines f
        wix = groupBy (groupon fst) . sort $  zip (map sort words )  words
        mxl = maximum  $ map length wix
    mapM_ (print . map snd)  . filter ((== mxl) . length) $ wix