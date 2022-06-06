{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Abelian where
import Control.Monad.Reader(asks,MonadReader (..),ReaderT,runReaderT, MonadTrans)
import Control.Monad.ST (runST,ST)
import Control.Monad.State (evalState,forM_,lift,MonadState(..),StateT,modify,when)
import Data.Array.ST (freeze,readArray,STUArray,thaw,writeArray)
import Data.Array.Unboxed (array,assocs,bounds,UArray,(!))
import Data.Word (Word32)
import System.IO (hPutStr,hPutStrLn,IOMode(WriteMode),withFile)
import Text.Printf(printf)

type Point = (Int,Int)
type ArrayST s = STUArray s Point Word32
type ArrayU = UArray Point Word32

newtype M s a = M (ReaderT (S s) (StateT [Point] (ST s))  a)
    deriving (Functor ,Applicative ,Monad,MonadReader (S s),MonadState [Point])

data S s = S 
    { bMin :: !Point
    , bMax :: !Point
    , arr  :: !(ArrayST s)
    }
