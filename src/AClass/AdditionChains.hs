module AClass.AdditionChains where
import Data.List(union)
import Text.Printf (printf)

total :: (Eq a, Num a) => [a] -> [a]
total [] = []
total (x:xs) = brauer (x:xs) `union` total xs

brauer :: Num a => [a] -> [a]
brauer [] = []
brauer (x:xs) = map (+x) (x:xs)

chains :: (Num a, Ord a) => ([a] -> [a]) -> a -> [[a]]
chains _ 1 = [[1]]
chains sums n  =  go [[1]]
    where   go chs =let next = chs >>= step
                        complete = filter ((== n) . head) next
                    in if null complete then go next else complete
            step chs = (: chs) <$> filter (\s -> s>head chs && s<=n) (sums chs)
isBrauer :: (Eq a, Num a) => [a] -> Bool
isBrauer [] = True
isBrauer [_] = True
isBrauer [_,_] = True
isBrauer (x:y:xs) = (x-y) `elem` (y:xs) && isBrauer(y:xs)

runTask :: Int -> IO ()
runTask n= 
    let ch = chains brauer n 
    in do
        printf "L(%d) = %d\n" n (length (head ch) -1)
        printf "Brauer chains(%i)\t: count = %i\tEx:  %s\n" n (length ch) (show $ reverse $ head ch)
        putStrLn "Non-Brauer analysis suppressed\n"
runMain = mapM_ runTask [ 47,79,191,382,379]
