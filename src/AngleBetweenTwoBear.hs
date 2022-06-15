module AngleBetweenTwoBear where

import Control.Monad(join)
import Data.Bifunctor(bimap)
import Text.Printf(printf)

type Radians = Float
type Degree  = Float

bearingDelta :: (Radians,Radians) -> Radians
bearingDelta (a,b) = sign * acos ((ax * bx) + (ay * by))
    where (ax,ay) = (sin a,cos a)
          (bx,by) = (sin b,cos b)
          sign 
            | ((ay*bx) - (by * ax)) > 0 =1
            | otherwise = -1

degree :: Radians -> Radians
degree = (/ pi) . (180 *)
radians :: Degree -> Radians
radians = (/ 180) . (pi *)

angleBetweenDegree :: (Degree,Degree) -> Degree
angleBetweenDegree = degree . bearingDelta . join bimap radians

runMain :: IO ()
runMain = do
    putStrLn . unlines $ 
        fmap ( uncurry  (printf "%6.2f. - %6.2f -> %7.2f")
                <*> angleBetweenDegree
             ) 
             [(20.0,45.0),(-45.0,45.0),(-85.0,90)]
