module AClass.ASKPrimes where
-- (x-1) ^ p - (x^p -1)
expand :: Int -> [Int]
expand p = scanl (\z i -> z * (p -i +1) `div` i ) 1 [1..p]

test :: Int -> Bool
test p | p < 2 = False
       | otherwise = and [n `mod` p == 0 | n <- init. tail $ expand p]
printPoly [1] ="1"
printPoly p = concat [unwords [pow i,sgn (l-i), show (p!!(i-1))] | i<- [l-1,l-2 ..1]]
    where   l = length p
            sgn i = if even i then "+" else "-"
            pow i = take i "x^" ++ if i > 1 then show i else ""
runMain = do
    putStrLn "--p: (x-1)^p for small p"
    putStrLn $ unlines [show i ++ ": " ++ printPoly (expand i) | i <- [0..10]]
    putStrLn "-- Primes up to 100:"
    print (filter test [1..100])