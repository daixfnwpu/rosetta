module TwoZeroFourEightGame where
import Data.List
import Data.Random ( randomElement,sample, RVar, runRVarT, Normal (StdNormal), Sampleable (sampleFrom) )
import Control.Lens
    ( (&),
      (^?),
      (.~),
      Ixed(ix),
      ReifiedTraversal(Traversal),
      ReifiedTraversal' )
import Data.Random.Distribution.Categorical
import Foreign (pooledNewArray0)
import Data.Maybe
import System.Console.ANSI
import GHC.IO.Handle (hSetEcho, hSetBuffering)
import Control.Monad (replicateM, replicateM_, when)
import System.IO
import Control.Monad.Reader (ReaderT(runReaderT), MonadIO (liftIO))
import System.Random.Stateful (globalStdGen)
numColors =
 [(0,"\ESC[38;5;234;48;5;250m     ")
 ,(2,"\ESC[38;5;234;48;5;255m  2  ")
 ,(4,"\ESC[38;5;234;48;5;230m  4  ")
 ,(8,"\ESC[38;5;15;48;5;208m  8  ")
 ,(16,"\ESC[38;5;15;48;5;209m  16 ")
 ,(32,"\ESC[38;5;15;48;5;203m  32 ")
 ,(64,"\ESC[38;5;15;48;5;9m  64 ")
 ,(128,"\ESC[38;5;15;48;5;228m 128 ")
 ,(256,"\ESC[38;5;15;48;5;227m 256 ")
 ,(512,"\ESC[38;5;15;48;5;226m 512 ")
 ,(1024,"\ESC[38;5;15;48;5;221m 1024")
 ,(2048,"\ESC[38;5;15;48;5;220m 2048")
 ,(4096,"\ESC[38;5;15;48;5;0m 4096")
 ,(8192,"\ESC[38;5;15;48;5;0m 8192")
 ,(16384,"\ESC[38;5;15;48;5;0m16384")
 ,(32768,"\ESC[38;5;15;48;5;0m32768")
 ,(65536,"\ESC[38;5;15;48;5;0m65536")
 ,(131072,"\ESC[38;5;15;48;5;90m131072")
 ]

prob4 :: Double
prob4 = 0.1

type Position =  [[Int]]

combile,shift :: [Int] -> [Int]
combile(x:y:l) | x==y = (2*x) : combile l
combile(x:l) = x : combile l
combile [] = []
shift l = take  (length l) $ combile $ filter (>0) l ++  [0,0..]

reflect :: [[a]] -> [[a]]
reflect = map reverse

type Move = Position -> Position

left,right,up,down :: Move
left = map shift
right = reflect . left . reflect
up = transpose . left . transpose
down = transpose . right .transpose

progress :: Eq a => (a -> a) -> a -> Maybe a
progress f pos = if pos== next_pos then Nothing else Just next_pos where next_pos = f pos

go :: Position -> Maybe Move -> Maybe Position
go pos move = move >>= flip progress pos
lost,win :: Position -> Bool
lost pos = all isNothing [progress move pos | move <- [left,right,up,down]]
win = any $ any (>=2048)

indicesOf :: [a] -> [ReifiedTraversal' [a] a]
indicesOf l = [Traversal $ ix i | i<-[0..length  l -1]]

indices20f :: [[a]] -> [ReifiedTraversal' [[a]] a]
indices20f ls = [Traversal $ i .j | Traversal i<- indicesOf ls, let Just l = ls ^?i,Traversal j <- indicesOf l]

add2or4:: Position -> RVar Position
add2or4 pos = do
    xy <- randomElement [xy | Traversal xy <- indices20f pos,pos ^? xy == Just 0]
    a  <- categorical [(1-prob4,2),(prob4,4)]
    return $ pos & xy .~ a

play :: Position -> IO ()
play pos = do
    c <-getChar
    case go pos $ lookup c [('D', left),('C',right),('A',up),('B',down)] of
        Nothing -> play pos
        Just pos1 -> do
            pos2 <-  sampleFrom globalStdGen $ add2or4 pos1  
            draw pos2
            when (win pos2 && not (win pos)) $ putStrLn "you win! you may keep goning."
            if lost pos2 then putStrLn "You lost!"
                         else play pos2
runMain = do
    pos <- sampleFrom  globalStdGen  $ add2or4 $ replicate 4 (replicate 4 0)
    draw pos
    play pos
showTile x = fromJust (lookup x numColors) ++ "\ESC[B\^H\^H\^H\^H\^H     \ESC[A\ESC[C"
--showTile x = fromJust (lookup x numColors) -- ++ "\ESC[B\^H\^H\^H\^H\^H     \ESC[A\ESC[C"

draw pos = do
    setSGR [Reset]
    clearScreen
    hideCursor
    hSetEcho stdin False
    hSetBuffering  stdin NoBuffering
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "\ESC[38:5:234:48:5:248m"
    setCursorPosition 0 0
    replicateM_ 13 $ putStrLn $ replicate 26 ' '
    setCursorPosition 1 1
    putStrLn $ intercalate "\n\n\n\ESC[C" $ concatMap showTile `map` pos

