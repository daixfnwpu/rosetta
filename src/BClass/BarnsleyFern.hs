module BClass.BarnsleyFern
where
import Data.List
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import System.Random

type Pt = (Double,Double)
f1,f2,f3,f4 :: Pt->Pt
f1 (x,y) = (                    0,      0.16*y)
f2 (x,y) = (0.85 * x + 0.04*y    ,      -0.04*x+0.85*y + 1.60)
f3 (x,y) = (0.20 * x - 0.26*y    ,      0.23*x+0.22*y + 1.60)
f4 (x,y) = (-0.15 * x + 0.28*y   ,      0.26*x+0.24*y + 0.44)

func p r | r < 0.01  = f1 p
         | r < 0.86  = f2 p
         | r < 0.93  = f3 p
         | otherwise = f4 p

fern = scanl' func (0,0) 

drawFern :: [Double] -> Int -> Diagram B
drawFern rs n = frame 0.5 . diagramFrom . take n $ fern rs
    where   diagramFrom = flip atPoints (repeat dot) . map p2
            dot = circle 0.005 # lc green

runMain = do
    rand <- getStdGen
    mainWith $ drawFern (randomRs (0,1) rand)