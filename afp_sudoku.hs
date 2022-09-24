{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
-- SUDOKU!

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

-- type Grid = [[Char]]

blank :: Grid
blank = replicate 9 (replicate 9 '.')

rows :: Matrix a -> [Row a]
rows m = m
-- rows = id identity function

-- Property : rows (rows m) = m
-- Property : rows . rows = id

cols :: Matrix a -> [Row a]
cols = transpose

-- Property : cols . cols = id

merge3Elements :: [a] -> [[a]]
merge3Elements [] = []
merge3Elements (a:b:c:ds) = [a,b,c]: merge3Elements ds

boxes :: Matrix a -> [Row a]
boxes = map concat . merge3Elements . concat . transpose . map merge3Elements

-- Property : boxs . boxs = id

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxes g)

-- Lecture 2

-- Making choices

type Choices = [Value]
choices :: Grid -> Matrix Choices
choices g = map (map choice) g
            where
              choice v = if v == '.' then ['1'..'9'] else [v]

-- cartesian product
cp :: [[a]] -> [[a]]
cp [] = [[]]
-- WOW!
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse m = cp (map cp m)

solve :: Grid -> [Grid]
solve = filter valid . collapse . choices -- ~9^51 options on the easy grid
-- solve g = fulter valid (collapse (choice g))

-- will take too long! Let's prune
-- my solutions for removing taken fields
removeTakenChoices :: Row Choices -> Choices -> Row Choices
removeTakenChoices choices taken = [if length x > 1 then filter (`notElem` taken) x else x | x <- choices]

reduce :: Row Choices -> Row Choices
reduce xs = removeTakenChoices xs takenChoices
            where takenChoices = concat (filter ((== 1) . length) xs)

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
        where pruneBy f = f . map reduce . f

solve2 = filter valid . collapse . prune . choices -- ~10^24 options on the easy grid

-- add repeated pruning
-- find fixPoint
-- This is a cool way of replacing a "while" loop
fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
          where x' = f x

solve3 = filter valid . collapse . fix prune . choices

-- Lecture 3
-- Blocked matrices!
-- "void": at least one cell has no choices
void :: Matrix Choices -> Bool
void m = any (any null) m

-- safe: when there are no duplicate single entries in a row of choices
noDuplicates :: Row Choices -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = (x `notElem` xs) && noDuplicates xs

consistent :: Row Choices -> Bool
consistent choices = noDuplicates (filter ((== 1) . length) choices)

safe :: Matrix Choices -> Bool
safe m = all consistent (rows m)
          && all consistent (cols m)
          && all consistent (boxes m)

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

single :: [a] -> Bool
single [_] = True
single _ = False

updateRow :: Row Choices -> Int -> Value -> Row Choices
updateRow row colIdx value = map (\ (col, currColIdx) -> if currColIdx == colIdx then [value] else col) (zip row [0..])

collapseCell :: Matrix Choices -> Int -> Int -> Value -> Matrix Choices
collapseCell m rowIdx colIdx choice = map (\ (row, currRowIdx) -> if currRowIdx == rowIdx then updateRow row colIdx choice else row) (zip m [0..])

expand :: Matrix Choices -> [Matrix Choices]
-- TODO: implement: expands the first cell that hs multiple options
expand m = [collapseCell m (fromJust rowIndex) (fromJust colIndex) option | option <- options ]
           where
             rowIndex = findIndex (any ((> 1) . length)) m
             colIndex = findIndex ((> 1) . length) (m !! fromJust rowIndex)
             options = (m !! fromJust rowIndex) !! fromJust colIndex



search :: Matrix Choices -> [Grid]
search m | blocked m = []
         | all (all single) m = collapse m
         | otherwise = [g | m' <- expand m, g <- search (prune m')]

solve4 = search . prune . choices


test = expand [ [['1', '2', '3'], ['1'], ['2', '3']], [['4'], ['5'], ['6']], [['7'], ['8'], ['9']] ]

