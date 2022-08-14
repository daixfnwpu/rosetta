{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module AClass.AnimatePendulum where

import Graphics.Gloss
    ( white,
      circleSolid,
      pictures,
      polygon,
      simulate,
      Display(InWindow),
      Picture(Text, Line, Translate, Scale) )
g_ = -9.8 :: Float
v_0 = 0    ::Float
a_0 = 0 / 180 *pi :: Float
dt = 0.01     :: Float

t_f = 15      :: Float

l_ = 200      :: Float

type Pendulum = (Float,Float,Float)

movePendulum :: Float -> Pendulum -> Pendulum
movePendulum dt (l,v,a) = (l,v_2,a + v_2 / l * dt * 10)
    where v_2 = v + g_ * cos a * dt

renderPendulum :: Pendulum -> [Picture]
renderPendulum (l,v,a) = map (uncurry  Translate newOrigin)
                            [Line  [(0,0),( l* cos a, l* sin a)]
                            ,polygon [(0,0),(-5,8.66),(5,8.66)]
                            ,Translate (l * cos a)  (l*sin a) (circleSolid (0.04*l_))
                            ,Translate (-1.1*l) (-1.3*l) (Scale 0.1 0.1 (Text currSpeed))
                            ,Translate (-1.1*l) (-1.3*l + 20) (Scale 0.1 0.1 (Text currAngle))
                            ]
    where
        currSpeed = "Speed (pixels/s)= " ++ show v
        currAngle = "Angle (deg) = " ++ show (90 + a / pi * 180)
newOrigin :: (Float, Float)
newOrigin = (0,l_ /2)

windowSize :: (Int, Int)
windowSize = (300 + 2 * round (snd newOrigin)
              ,200 + 2 * round (snd newOrigin))

runMain :: IO ()
runMain = do
    simulate window backgroud fps initialstate render update
        where window = InWindow "Animate a pendulum" windowSize (40,40)
              backgroud = white
              fps = round (1/dt)
              initialstate = (l_,v_0,a_0)
              render xs = pictures $ renderPendulum xs
              update _  = movePendulum

