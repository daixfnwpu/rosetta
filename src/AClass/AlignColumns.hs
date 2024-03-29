{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AClass.AlignColumns where
import Data.List(unfoldr,transpose)
import Control.Arrow(second)
import AClass.NineBillonNamesGod (rows)

dat :: String
dat ="Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n" ++
    "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" ++
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n" ++
    "column$are$separated$by$at$least$one$space.\n" ++
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$\n" ++
    "justified,$right$justified,$or$center$justified$within$its$column."
brkdwn :: String -> [String]
brkdwn = takeWhile (not . null) . unfoldr (Just . second (drop 1) . span ('$' /=))
-- span == splite a list use the '$'.
format :: Char -> String -> [String]
format j ls = map (unwords . zipWith align colw) rows
    where
        rows = map brkdwn $ lines  ls
        colw = map (maximum . map length) . transpose $ rows
        align cw w =
            case j of
                'c' -> replicate l ' ' ++ w ++ replicate r ' '
                'r' -> replicate dl ' '  ++ w
                'l' -> w ++ replicate dl  ' ' 
            where
                dl = cw - length w
                (l,r)= (dl `div` 2,dl-1)
runMain :: IO ()
runMain = do
    mapM_ putStrLn $ format 'l' dat