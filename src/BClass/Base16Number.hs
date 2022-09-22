module BClass.Base16Number where

import Data.List (intercalate,transpose)
import Data.List.Split (chunksOf)
import Text.Printf(printf)

p n = 9 < n && (9 < n `rem` 16 || p (n `quot` 16))

table alignment gap rows = 
    unlines $ fmap (intercalate gap . zipWith (`alignment` ' ') colWidths) rows
    where
        colWidths = maximum . fmap length <$> transpose rows

justifyRight n c = (drop . length ) <*> (replicate n  c <>)

runMain = let upperLimit = 500
              xs = [show x | x <- [0 .. upperLimit], p x]
        in mapM_ 
            putStrLn 
            [ show (length xs)
                <> " matches up to "
                <> show upperLimit
                <> ":\n",
                table justifyRight " " $ chunksOf 15 xs
            ]
