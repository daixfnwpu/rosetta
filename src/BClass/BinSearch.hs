module BClass.BinSearch where
import Data.Array (Array,Ix,(!),listArray,bounds)

bSearch :: Integral a => (a -> Ordering) -> (a,a) -> Maybe a
bSearch p (low,high) 
    | high < low = Nothing
    | otherwise =
        let mid = (low + high) `div` 2
        in case p mid of
            LT -> bSearch p (low ,mid -1)
            GT -> bSearch p (mid + 1,high)
            EQ -> Just mid
bSearchArray :: (Ix i, Integral i ,Ord e) => Array i e -> e -> Maybe i
bSearchArray a x = bSearch (compare x . (a !)) (bounds a)
axs
  :: (Num i, Ix i)
  => Array i String
axs =
  listArray
    (0, 11)
    [ "alpha"
    , "beta"
    , "delta"
    , "epsilon"
    , "eta"
    , "gamma"
    , "iota"
    , "kappa"
    , "lambda"
    , "mu"
    , "theta"
    , "zeta"
    ]

runMain :: IO ()
runMain = 
    let e = "mu"
        found = bSearchArray axs e
    in  putStrLn $
        '\'' : e ++ 
        case found of
                        Nothing -> " ' Not Founded"
                        Just x -> "' found at index " ++ show x    