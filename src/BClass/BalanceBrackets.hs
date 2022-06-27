{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BClass.BalanceBrackets where
import Control.Monad ( replicateM, zipWithM ) 
import Text.Printf (printf)
import System.Random ( Random(randomR), RandomGen, newStdGen ) 
import Control.Monad.ST ( runST )
import Data.List (mapAccumL)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M

-- isMatching :: String -> Bool
-- isMatching = null . foldl aut []
--     where aut ('[': s) ']' = s
--           aut ('{':s) '}'  = s
--           aut s x         = x : s

-- brackets :: [String]
-- brackets = filter isMatching $ [1..] >>= (`replicateM` "[]{}")

isBalanced :: String -> Maybe Int
isBalanced = bal (-1) 0
    where bal _ 0 [] = Nothing
          bal i _ [] = Just  i
          bal i (-1) _ =Just i
          bal i n ('[':bs) = bal (i+1) (n+1) bs
          bal i n (']':bs) = bal (i+1) (n-1) bs

check :: String -> IO()
check s = maybe (good s) (bad s) (isBalanced s)
    where good  = printf "Good \"%s\"\n"
          bad s n = printf "Bad \"%s\"\n%*s^\n" s (n+6) " "

pairs :: (Enum a,Random a,RandomGen g)
    => a -> a -> g -> [(a,a)]
pairs l u r = snd $ mapAccumL step  r [ l.. pred u]
    where step r i = 
            let (j,r') = randomR (i,u) r
            in (r',(i,j))

shuffle :: (RandomGen g) => String -> g -> String
shuffle  xs r = 
    V.toList . runST $  do v <- V.unsafeThaw $ V.fromList xs
                           mapM_ (uncurry $ M.swap v) $ pairs 0 (M.length v -1) r
                           V.unsafeFreeze v

-- runMain :: IO ()
-- runMain = do 
--     let bs = cycle "[]"
--     rs <- replicateM 10 newStdGen
--     zipWithM (\n r -> check $ shuffle (take n bs) r) [0,2 ..] rs