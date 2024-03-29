{-# LANGUAGE LambdaCase #-}
module AClass.TwentityFour where
import Data.List (sort)
import Data.Char(isDigit)
import Data.Maybe (fromJust)
import Control.Monad(foldM)
import System.Random (randomRs,getStdGen)
import System.IO (hSetBuffering,stdout,BufferMode(NoBuffering))
import GHC.Base (IO(IO))

runMain = do
    hSetBuffering stdout NoBuffering
    mapM_ putStrLn  ["THe 24 Game\n","Given four digits in the range 1 to 9 ","Use the +,-,* and / operators in reverse polish notation","To show how to make an answer 24.\n"]
    digits <- fmap (sort . take 4 .randomRs (1,9))   getStdGen :: IO [Int]
    putStrLn $ "Your digits:" ++ unwords ( fmap show digits)
    guessLoop digits
    where guessLoop digits = putStr "Your expression: " >> fmap (processGuess digits . words) getLine >>=
            either (\m -> putStrLn m >> guessLoop digits) putStrLn 

processGuess _ [] = Right ""
processGuess digits xs 
    | not matches = Left "Wrong digits used"
    where
        matches = digits == (sort . fmap read $ filter (all isDigit) xs)
processGuess digits xs   = calc xs >>=check
    where
        check 24 = Right "Correct"
        check x = Left (show (fromRational (x :: Rational)) ++ " is wrong")
calc xs = foldM simplify [] xs >>= \case
                                        [n] -> Right  n
                                        _   -> Left "Too few operators" 

simplify (a:b:ns) s 
    | isOp s = Right ((fromJust $ lookup s ops) b a : ns)
simplify _ s 
    | isOp s = Left ("Too few values before " ++ s)
simplify ns s 
    | all isDigit s = Right (fromIntegral (read s) : ns)
simplify  _ s = Left ("Unrecongnized symbo: " ++ s)

isOp v = elem v $ fmap fst ops
ops = [("+",(+)),("-",(-)),("*",(*)),("/",(/))] 