module AClass.ArchimedeanSpiral where
import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Prelude hiding (lines)
archimedeanPoint :: Float -> Float -> Float -> Point
archimedeanPoint a b t = (x,y)
    where r = a + b * t
          x = r * cos t
          y = r * sin t
--drawPoint points =  
points :: [Point]
points = map (archimedeanPoint 0 10) [ 0, 0.01 .. 60]

spiral :: Picture
spiral =  line points

runMain :: IO ()
runMain = do display (InWindow "AchimedeanSpiral" (400, 400) (20, 20)) white spiral

