import Data.List
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
