module AClass.Accumulator where

import Control.Monad.ST
import Data.STRef



accumulator :: Num  b => b -> ST s (b -> ST s b)
accumulator sum0 = do
    sum <- newSTRef  sum0
    return $ \n -> do
        modifySTRef sum (+ n)
        readSTRef sum

runMain :: IO()
runMain = print foo
    where foo = runST $ do
                    f <- accumulator (1::Float)
                    f 5.0
                    accumulator 3
                    f 2.3