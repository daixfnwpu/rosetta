module AVLTree where
data Tree a 
    = Leaf 
    | Node 
        Int
        (Tree a)
        a 
        (Tree a)
    deriving (Show,Eq)

foldTree :: [Integer] -> Tree Integer
foldTree = foldr insert Leaf 
insert  :: Ord a =>  a -> Tree a -> Tree a
insert v Leaf = Node 1 Leaf v Leaf
insert v t@(Node n left v_ right)
    | v_ < v = rotate $ Node n left v_ (insert v right)
    | v_ > v = rotate $ Node n (insert v left) v_ right
    | otherwise = t

rotate :: Tree a -> Tree a
rotate Leaf = Leaf
rotate (Node h nl@(Node lh ll lv lr) v nr@(Node rh rl rv rr)) 
    | lh - rh > 1 && height ll - height lr > 0 = 
        Node lh ll lv (Node (depth nr lr) lr v nr)
    | rh - lh > 1 && height rr - height rl > 0 =
        Node rh (Node (depth nl rl) nl v rl ) rv rr 

rotate (Node h nl@(Node lh ll lv lr@(Node rlh rll rvl rrl)) v nr@(Node rh rl rv rr)) 
    | lh - rh > 1 = 
        Node  h (Node (rlh + 1) (Node (lh -1) ll lv rll) rvl rrl) v nr

rotate (Node h l v (Node rh (Node lh ll lv lr) rv rr))
    | rh - height l > 1 = 
        Node h l v (Node (lh + 1) ll lv (Node (rh - 1) lr rv rr))

rotate (Node h l v r) =
    let (l_ ,r_) = (rotate l, rotate r)
    in Node (depth l_ r_) l_ v r_

height :: Tree a -> Int
height Leaf = -1
height (Node h _ _ _) = h

depth :: Tree a -> Tree a -> Int
depth a b = succ (max (height a) (height b))

draw :: Show a => Tree a -> String
draw t = '\n' : draw_ t 0 <> "\n"
    where 
        draw_ Leaf _ = []
        draw_ (Node h l v r) d = draw_ r (d+1) <> node <> draw_ l (d+1)
            where
                node = padding d <> show (v,h) <> "\n"
                padding n = replicate (n * 4) ' '

runMain :: IO ()
runMain = putStr $ draw $ foldTree [1.. 31]