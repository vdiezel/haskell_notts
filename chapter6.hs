fac :: Int -> Int
fac n = product [1..n]

facRecursive 0 = 1
facRecursive n = n * facRecursive (n - 1)

-- nothing new here

-- Execises
and1 :: [Bool] -> Bool
and1 [] = True
and1 (x:xs) = x && and1 xs

concat1 :: [[a]] -> [a]
concat1       [] = []
concat1 (xs:xss) = xs ++ concat xss

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n a = a : replicate (n-1) a

--(!!) :: [a] -> Int -> a

elem1 :: Eq a => a -> [a] -> Bool
elem1 a [] = False
elem1 a (x:xs) = a == x || elem1 a xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys  = ys
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys)
                               else y: merge (x:xs) ys

