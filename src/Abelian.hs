{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Abelian where
import Control.Monad.Reader(asks,MonadReader (..),ReaderT,runReaderT, MonadTrans)
import Control.Monad.ST (runST,ST)
import Control.Monad.State (evalState,forM_,lift,MonadState(..),StateT,modify,when, evalStateT)
import Data.Array.ST (freeze,readArray,STUArray,thaw,writeArray)
import Data.Array.Unboxed (array,assocs,bounds,UArray,(!))
import Data.Word (Word32)
import System.IO (hPutStr,hPutStrLn,IOMode(WriteMode),withFile)
import Text.Printf(printf)
--import GHC.ST (liftST)

type Point = (Int,Int)
type ArrayST s = STUArray s Point Word32 -- mutable,unboxed non-strict arrays in the ST monad. s : the state variable argument for the ST type.i: the index type of the array. e:
                                         -- e: the element type of the array.Only centain element types are supported. STUArray s i e;
type ArrayU = UArray Point Word32 -- Arrays with unboxed elements. i: the index of the array. e : the element type of the array. UArray i e;

newtype M s a = M (ReaderT (S s) (StateT [Point] (ST s))  a)
    deriving (Functor ,Applicative ,Monad,MonadReader (S s),MonadState [Point])
-- MonadReader is the Reader monad (also called the Evvironment monad). Represents a computation, which can read values from a shared environment. pass values from function to 
-- function . and execute sub-computations in a modified environment. Using Reader monad for such computations is often clearer and easier than useing the state monad.
data S s = S
    { bMin :: !Point
    , bMax :: !Point
    , arr  :: !(ArrayST s)
    }

runM :: M s a -> S s -> [Point] -> ST s a
runM (M m) = evalStateT . runReaderT m

simulate :: ArrayU  -> ArrayU
simulate a = runST $ simulateST a

simulateST :: forall s .  ArrayU -> ST s ArrayU
simulateST a = do
    let (p1,p2) = bounds a
        s       = [p | (p,c) <- assocs a,c >=4]
    b <- thaw a :: ST s (ArrayST s)
    let st = S {
        bMin = p1
       ,bMax = p2
       ,arr =b
    }
    runM simulateM st s

simulateM :: forall s . M s ArrayU
simulateM = do
    ps <-get
    case ps of
        [] -> asks arr >>= liftST . freeze
        p : ps' -> do
            c <- changeArr p $ \x -> x - 4
            when ( c < 4 ) $ put ps'
            forM_ [north,east,south,west] $ inc . ($  p)
            simulateM

liftST :: ST s a -> M s a
liftST = M . lift . lift

inc :: Point -> M s ()
inc p =  do
    b <- inBounds p
    when b $ do
        c <- changeArr p succ
        when (c==4) $ modify (p :)

inBounds :: Point -> M s Bool
inBounds  p =   do
    st <- ask
    return $ p >= bMin st && p <= bMax st

west :: Point -> Point
west (x,y) = (x-1,y)
south :: Point ->Point 
south (x,y)= (x,y-1)
east :: Point -> Point
east (x,y)= (x+1,y)
north :: Point -> Point 
north (x,y) =  (x,y+1)

changeArr :: Point -> (Word32 -> Word32) -> M s Word32
changeArr p f = do
    a <- asks arr
    oldC <- liftST $ readArray a p
    let newC = f oldC
    liftST $ writeArray a  p newC
    return newC
initArray size height = array 
      ((-size,-size), (size,size)) 
      [((x,y), if x==0 && y==0 then height else 0) | x<-[-size .. size], y<-[-size .. size]]
toPGM :: ArrayU -> FilePath -> IO ()
toPGM a fp = withFile fp WriteMode $ \h -> do
    let ((x1,y1),(x2,y2)) = bounds a
        width = x2 - x1 +1
        height = y2 -y1 +1
    hPutStrLn h "p2"
    hPutStrLn h $ show width ++ " " ++ show height
    hPutStrLn h "3"
    forM_ [y1..y2]  $ \y -> do
        forM_ [x1..x2] $ \x -> do
            let c = min 3 $ a ! (x,y)
            hPutStr h $ show c ++ " "
        hPutStrLn h ""

runMain size height = do
    printf "size = %d, height = %d\n" size height
    let a = initArray size height
        b = simulate a
        fp = printf "sandpile_%d_%d.pgm" size height
    toPGM b fp
    putStrLn $ "write image to " ++ fp