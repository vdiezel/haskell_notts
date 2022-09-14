-- Exercises for recursion

-- (1) curried
--
--
--
-- (2)

mapAndFilter xs f p = map f (filter p xs)

map' f = foldr (\x ys -> f x :ys) []

-- declare my own type
-- type declarations (important: "declare" instead if define)

type String = [Char]
type Pos = (Int, Int)

origin :: Pos
origin = (0, 0)

left :: Pos -> Pos
left (x, y) = (x - 1, y)

-- parameters

type Pair a = (a, a)
mult :: Pair Int -> Int
mult (m, n) = m * n
copy :: a -> Pair a
copy x = (x, x)

-- types cannot be recursive, only nested

type Trans = Pos -> Pos -- this is ok!
-- type Tree = (Int, [Tree]) -- not ok

-- data declarations

data Bool = False | True  --enum all possible values a.k.a constructors
-- always upper case
-- parameters

data Maybe a = Nothing | Just a
-- recursive
data Nat = Zero | Succ Nat

-- arithmetic expression

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr

-- Exercises
mul :: Nat -> Nat -> Nat
mul Zero n = Zero
mul (Succ m) n = add (mul n m) m

-- (3)
data Tree a = Leaf a | Node (Tree a) (Tree a)




