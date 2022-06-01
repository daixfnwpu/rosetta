{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module NineNineBottleOfBeer where
import qualified Data.Char as Char
import Language.Haskell.TH
import Control.Monad.Writer

runBrevity = mapM_ (putStrLn . beer) [99,98 .. 0]
beer 1 =" 1 bottle of beer on the wall\n 1 bottle of beer \n Take one down ,pass it around"
beer 0 ="better go to the store and buy some more."
beer v = show v ++ " bottles of beer on the wall\n"
                ++ show v
                ++ " bottles of beer\n Take one down, pass it around\n"
                ++ head (lines $ beer $ v-1) ++"\n"
runList = putStr $ concat
        [up (bob n) ++ wall ++ "," ++ bob n ++".\n" ++ pass n ++ bob (n-1) ++ wall ++ ".\n\n" | n <- [99,98 .. 0]]
        where bob n = num n ++ " bottle" ++ s n ++ " of beer"
              wall =" on the wall"
              pass 0 = "Go to the store and buy some more,"
              pass _ = "Take one down and pass it around,"
              up (x :xs) = Char.toUpper x : xs
              num (-1) = "99"
              num 0 = "no more"
              num n = show n
              s 1 = ""
              s _ ="s"
runTH = putStr songString
songString = $(
    let sing = tell
        someBottles 1 = "1 bottle of beer"
        someBottles n = show n ++ " bottles of beer"
        bottlesOfBeer n = (someBottles n ++)
        verse n = do
            sing $ n `bottlesOfBeer` "on the wall\n"
            sing $ n `bottlesOfBeer` "\n"
            sing "Take one down,pass it around\n"
            sing $ (n-1) `bottlesOfBeer` "on the wall\n\n"
        song = execWriter $ mapM_ verse [99,98..1]
    in return $ LitE $ StringL  song)