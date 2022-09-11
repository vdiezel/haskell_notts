
-- lists

-- [1, 2, 3, 4]
-- [] empty list
-- [1] singleton list

-- ++
-- list processor
-- [1, 2, 3] ++ [4, 5] = [1, 2, 3, 4, 5] concatenation!

-- :
-- list processor
-- 1: [2, 3, 4] = [1, 2, 3, 4] prepends a single element
-- f(x:xs) breaks out the first element from lists

qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
               where
                 ys = [a | a <- xs, a <= x]
                 zs = [b | b <- xs, b > x]

main = print (qsort [3, 1, 2, 4])
