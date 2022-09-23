module BClass.BellNumber where

bellTri :: [[Integer]]
bellTri = let f xs = (last xs ,xs)
    in map snd (iterate (f . uncurry (scanl (+))) (1,[1]))

bell = map head bellTri

runMain = do 
    putStrLn "First 10 rows of Bell's Triangle:"
    mapM_ print (take 10 bellTri)
    putStrLn "\nFirst 15 Bell Number:"
    mapM_  print (take 15 bell)
    putStrLn "\n 50th Bell Number:"
    print (bell !! 49)
