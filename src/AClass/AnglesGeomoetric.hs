{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module AClass.AnglesGeomoetric where

import Text.Printf

class (Num a,Fractional a,RealFrac a) => Angle a where
    fullTurn :: a 
    mkAngle  :: Double -> a
    value    :: a -> Double
    fromTurn :: Double -> a
    toTurn   :: a -> Double
    normalize :: a -> a
    fromTurn t = angle t * fullTurn
    toTurn   a = value $ a / fullTurn
    normalize a = a `modulo` fullTurn
        where modulo x r | x == r = r
                         | x < 0 = signum x * abs x `modulo` r
                         | x >= 0 = x -fromInteger (floor (x/r)) * r
angle :: Angle a => Double -> a
angle = normalize . mkAngle

from :: forall a b . (Angle a,Angle b) => a -> b
from = fromTurn . toTurn

to  :: forall b a . (Angle a,Angle b)=> a -> b
to = fromTurn . toTurn

newtype Rad = Rad Double
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac ,Floating)

instance Show Rad where
    show (Rad 0) = printf "<0"
    show (Rad r) = printf "<%0.3f" r
instance Angle Rad where
    fullTurn = Rad 2*pi
    mkAngle = Rad
    value (Rad r) = r
newtype Deg = Deg Double
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating)
instance Show Deg where
    show (Deg 0) = printf "0"
    show (Deg d) = printf "%.3go" d
instance Angle Deg where
    fullTurn = Deg 360
    mkAngle  = Deg
    value (Deg d) = d
newtype Mil = Mil Double
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating)
instance Show Mil where
    show (Mil 0) = printf "0m"
    show (Mil m) = printf "%.3m" m
newtype Grad = Grad Double
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating)
instance Show Grad where
    show (Grad 0) = printf "0m"
    show (Grad g)  = printf "%.3gm" g


instance Angle Grad where
    fullTurn = Grad 400
    mkAngle  = Grad
    value (Grad g) = g
instance Angle Mil where
    fullTurn = Mil 6400
    mkAngle = Mil
    value (Mil m) = m

newtype Slope = Slope Double 
    deriving (Eq,Ord,Num,Real,Fractional,RealFrac,Floating)
instance Show Slope where
    show (Slope 0) = printf "0%"
    show (Slope m) = printf "%.g" (m*100) ++ "%"
instance Angle Slope where
    fullTurn = undefined
    mkAngle  = Slope
    value (Slope t) = t
    toTurn = toTurn @Rad . angle . atan . value
    fromTurn = angle . tan . value . fromTurn @Rad
    normalize = id

runMain :: IO ()
runMain = do
    let xs = [-2,-1,0,1,2,6.2831853]
    putStrLn "converting to radians"
    print $ to @Rad . angle @Rad <$> xs