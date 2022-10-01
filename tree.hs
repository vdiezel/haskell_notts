-- How do trees work?

-- a rose tree that can be empty:
data Tree a = Empty | Node a [Tree a]
                deriving (Show)

addElem :: Tree a -> a -> Tree a
addElem Empty elem       = Node elem []
addElem (Node a xs) elem = Node a (Node elem []:xs)

tree = foldl addElem Empty [0..10]

-- traversal

dfs :: Tree Int -> [Int]
dfs Empty = []
-- dfs (Node num []) = [num]
dfs (Node num nodes) = concat (map dfs nodes) ++ [num]

testDFS = dfs tree

