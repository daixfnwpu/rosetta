{-# LANGUAGE LambdaCase #-}
module AClass.ThroundPrisoners where
import System.Random
import Control.Monad.State

numRuns = 100000
numPrisoners = 100
numDrawerTries = 50

type Drawers = [Int]
type Prisoner = Int
type Prisoners = [Int]

runMain = do
    gen <- getStdGen
    putStrLn $ "Chance of winning when choosing randomly " ++ show (evalState runRandomly gen)

runRandomly :: State StdGen Double
runRandomly =
    let runResults = replicateM  numRuns $ do
        drawers <- state $ shuffle [1.. numPrisoners]
        allM (\prisoner -> openDrawersRandomly drawers prisoner numDrawerTries) [1..numPrisoners]
    in  fmap  ((/ fromIntegral numRuns) . fromIntegral . sum . map fromEnum) runResults

openDrawersRandomly :: Drawers -> Prisoner -> Int -> State StdGen Bool
openDrawersRandomly drawers prisoner triesLeft = go triesLeft []
    where go 0 _ = return False
          go triesLeft seenDrawers =
              state (randomR (1, numPrisoners)) >>= \case 
                                                            x | x == prisoner -> return True
                                                              | x `elem` seenDrawers -> go triesLeft seenDrawers
                                                              | otherwise -> go (triesLeft -1 ) (x : seenDrawers)
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM func [] = return True
allM func (x:xs) = func x >>= \case
                                   True -> allM func xs
                                   _ -> return False -- >>= \res -> if res then allM func xs else return False

shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle list gen = (shuffleByNumbers numbers list,finalGen)
    where
        n = length list
        (numbers,finalGen) = randomLR n (0,n-1) gen
        shuffleByNumbers :: [Int] -> [a] -> [a]
        shuffleByNumbers [] _ = []
        shuffleByNumbers _ [] = []
        shuffleByNumbers (i:is) xs = let (start,x:rest) = splitAt (i `mod` length xs) xs
                                     in x : shuffleByNumbers is (start ++ rest)

randomLR :: Integral a => Random b => a -> (b,b) -> StdGen -> ([b],StdGen)
randomLR 0 range gen = ([],gen)
randomLR len range gen =
    let (x,newGen) = randomR range gen
        (xs,lastGen) = randomLR (len -1) range newGen
    in (x : xs,lastGen)