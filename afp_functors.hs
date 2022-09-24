-- MONADS (be the force with me)
-- Abstracting programming patterns

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = (n + 1) : inc ns

sqr:: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n ^ 2 : sqr ns

-- you see the pattern, Viktor?
-- let's see, here is a map:

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

inc' = map (+1)
sqr' = map (^2)

-- let's go deeper...
-- we could also map over other data structures

class Functor f where -- f must be a parametrized type, like a List of a
  fmap :: (a -> b) -> f a -> f b

-- Functor: from category theory

-- the list functor

instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map

-- the Maybe functor
data Maybe a = Nothing | Just a

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap g Nothing  = Nothing
  fmap g (Just x) = Just (g x)

-- the tree functor
data Tree a = Leaf a | Node (Tree a) (Tree a)
t :: Tree Int
t = Node (Leaf 1) (Leaf 2)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- Why use functors?
-- 1. Can use the same name, fmap, for functions
-- that are essentially the "same" (like "mapping" all values of a data structure)
-- 2. Can define generic functions that work for any functorial type

incF :: Functor f => f Int -> f Int
incF = fmap (+1)

-- I love functors
