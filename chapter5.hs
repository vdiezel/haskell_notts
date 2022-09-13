-- list comprehensions
-- [x^2 | x <- [1..5]]

-- the <- syntax is called generator

-- multiple var
-- [(x, y) | x <- [1, 2, 3], y <- [4, 5]]

-- depending generators

-- [(x, y) | x <- [1, 2, 3], y <- [x, 5]]

-- flat (concat)
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- guards (filter out values from a generator)
-- [x | x <- [1..10], even x]

factors:: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- use zip to create a list of adj pairs form a list
pairs xs = zip xs (tail xs)

-- get all positions of an element
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- Exercises

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n],
                       y <- [1..n],
                       z <- [1..n],
                       z^2 == x^2 + y^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Grahams solution
sp xs ys = sum [xs !! i * ys !! i | i <- [0..n-1]]
           where n = length xs

-- ok, his second, cleaner solution is mine :P
