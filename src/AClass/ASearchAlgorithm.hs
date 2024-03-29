{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE BlockArguments #-}
module AClass.ASearchAlgorithm where
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.List (unfoldr)
data PQueue a = EmptyQueue | Node (Int,a) (PQueue a) (PQueue a)
    deriving (Show,Foldable)
-- deriving clause . It specifies that we want the compiler to automatically generate instances of the Show and Foldable classes for our PQueue type.

newtype Graph n = Graph {links :: n-> Map n Int}
-- new type is used to one parameter and only one parameters Type.
instance  Semigroup (PQueue a) where
    h1@(Node (w1,x1) l1 r1) <> h2@(Node (w2,x2) l2 r2)
        | w1 < w2 = Node (w1,x1) (h2 <> r1) l1
        | otherwise = Node (w2,x2) (h1 <> r2) l2
    EmptyQueue <> h = h
    h <> EmptyQueue = h
-- instace this PQueue, if we do'nt imstance this class ,we can not use the <> associative.
entry :: Ord a => a -> Int -> PQueue a
entry x w = Node (w,x) EmptyQueue EmptyQueue

enque :: Ord a => PQueue a -> a -> Int -> PQueue a
enque q x w = if x `notElem` q
                    then entry x w <> q
                    else q
deque :: Ord  a => PQueue a -> Maybe (a , PQueue a)
deque q = case q of
    EmptyQueue -> Nothing
    Node (_,x) l r -> Just (x, l<>r)

grid :: Int -> Int -> Graph (Int,Int)
grid a b = Graph $ \(x,y) ->
                        let links = [((x+dx,y+dy),dx*dx + dy*dy) | dx <- [-1..1],dy <-[-1..1],not (dx==0 &&dy==0),0<=x+dx && x+dx <= a ,0<y+dy && y+dy <=b]
                        in Map.fromList links
withHole :: (Foldable t, Ord k) => Graph k -> t k -> Graph k
withHole (Graph g) ns = Graph $ \x ->
    if x `elem` ns
        then Map.empty
        else foldr Map.delete (g x) ns
get :: (Ord  k,Bounded a) => Map k a -> k -> a
get m x = Map.findWithDefault maxBound x m

set :: (Ord k,Bounded  a) => Map k a -> k -> a ->Map k a
set m k x = Map.insert k x m

data AstarData n = SetData { cameFrom :: Map n n
                            ,gScore   :: Map n Int
                            ,openSet  :: PQueue n}

findPath :: (Ord n, Bounded n) => Graph n -> (n -> n -> Int) -> n -> n -> [n]
findPath (Graph links) metric start goal = loop a0
    where
        a0 = SetData{
                         cameFrom = mempty
                        ,gScore   = Map.singleton start 0
                        ,openSet  = entry start (h start)
                    }
        h  = metric goal
        dist = get . links
        loop a = case deque (openSet a) of
            Nothing -> []
            Just (current,q') -> if current == goal
                                    then getPath (cameFrom a)
                                    else loop a'
                where
                    a' = Map.foldlWithKey go  a {openSet = q'} neighbours
                    neighbours = links current
                    go a n _ =
                        let g = get $ gScore a
                            trial_Score = g current + dist current n
                        in if trial_Score >= g n
                            then a
                            else SetData (set (cameFrom a) n current)
                                         (set (gScore a)  n trial_Score)
                                         (openSet a `enque` n $ trial_Score + h n)
        getPath m = reverse $ goal : unfoldr go goal
            where go n = (\x -> (x,x)) <$> Map.lookup n m

distL1 (x,y) (a,b) = max (abs $ x -a) (abs $ y-b)  

runMain = let
    g = grid 9 9 `withHole` wall
    wall = [(2,4),(2,5),(2,6),(3,6),(4,6),(5,6),(5,5),(5,4),(5,3),(5,2),(3,2),(4,2)]
    path = findPath g distL1 (1,1) (7,7)
    picture = [[ case (i,j) of
                    p | p `elem` path -> '*'
                      | p `elem` wall -> '#'
                      | otherwise -> ' ' 
                | i <- [0..8]] 
              | j <- [0..8]]
    in do
        print path
        mapM_ putStrLn picture
        putStrLn $ "Path score: " <> show (length path)