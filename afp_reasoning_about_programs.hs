-- 1. proofs about programs
-- 2. improve program efficiency
-- 3. case study: compiler correctnes
--
-- 1. (Induction - nothing new)
-- 2. A better reverse
-- by removing the ++ operator as it causes a qudratic time complexity
-- new time complexity is linear

reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x:xs) ys = reverse' xs (x:ys)

reverse xs = reverse' xs []

data Tree = Leaf Int | Node Tree Tree

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r -- append :

-- lets remove the use of the append operator (again using an accumulator)

-- flatten' t ns = flatten t ++ ns -- specification

-- induction on t

-- flatten' (Leaf n) ns
--   = flatten (Leaf n) ++ ns
--   = flatten (Leaf n) ++ ns
--   = [n] ++ ns
--   = n:ns
flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n:ns

-- inductive case
-- flatten' (Node l r) ns
-- = flatten (Node l r) ++ ns
-- = (flatten l ++ flatten r) ++ ns
-- = flatten l ++ (flatten r ++ ns)
-- = flatten l ++ (flatten' r ++ ns)
-- = flatten' l ((flatten' r ++ ns))

flatten' (Node l r) ns = flatten' l (flatten' r  ns)

flattenEfficient :: Tree -> [Int]
flattenEfficient t = flatten' t []

-- accumulator style of programming seems the way to go to
-- increate efficiency on functions that highly rely on "append"

-- 3. Case study: Compiler Correctness
-- Source Language

data Expr = Val Int | Add Expr Expr

-- Semantics

eval :: Expr -> Int
eval (Val n)   = n
eval (Add l r) = eval l + eval r

-- Target Language
-- a VM with a stack and memory

type Stack = [Int]
type Code  = [Op]
data Op    = PUSH Int | ADD

exec :: Code -> Stack -> Stack
exec [] s            = s
exec (PUSH n:c) s    = exec c (n:s)
exec (ADD:c) (m:n:s) = exec c (n+m : s)

-- Compiler
comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- What does it mean in our case for a compiler to be correct?

-- exec (comp e) [] = [eval e]
-- exec (comp e) s = eval e:s     generalisation
-- Expr ---> eval ---> Int
-- |                    |
-- |                    |
-- | comp               | (:s)
-- |                    |
-- v                    v
-- Code ------------> Stack
        --`exec` s

-- This is a commuting square
--
-- Compiler Correctness Specification
-- exec (comp e) s = eval e : s

-- Base case
-- exec (comp (Val n)) s
-- = exec [PUSH n] s
-- = n:s -- we are stuck, now lets go backwards from
--
-- = eval (Val n) :s
-- = n:s which is the same as L98
--
--
-- Inductive case
-- exec (comp (Add x y)) s
-- = exec (comp x ++ comp y ++ [ADD]) s
-- = exec (comp x ++ (comp y ++ [ADD])) s

-- Lemma
-- exec (c ++ d) s = exec d (exec c s)

-- = exec (comp x ++ (comp y ++ [ADD])) s
-- = exec (comp y ++ [ADD]) (exec (comp x) s)
-- = exec (comp y ++ [ADD]) (eval x : s)
-- = exec [ADD] (exec (comp y) (eval x:s)
-- = exec [ADD] (eval y: (eval x:s))
-- = (eval x + eval y) : s
-- = eval (Add x y) : s !QED

-- takes too long. Lets use accumulator style

compAcc :: Expr -> Code
compAcc e = comp' e []

-- Good Exercise: Deduce the recursive case of comp'
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD:c))

-- New Compiler Correctness
-- exec (comp' e c) s = exec c (eval e : s)

-- Base Case
-- exec (comp' (Val n) c) s
-- exec (PUSH n : c) s
-- exec c (n:s)
-- exec c (eval (Val n):s) (Expanded n) QED
--
-- Inductive Case
-- exec (comp' (Add x y) c) s
-- = exec (comp' x (comp' y (Add:c))) s
-- = exec (comp' y (ADD:c)) (eval x:s)
-- = exec (ADD:c) (eval y: eval x : s)
-- = exec c ((eval x + eval y): s)
-- = exec c (eval (Add x y): s)

-- Benefits:
-- 1. 22 steps -> 8 steps
-- 2. 2 lemmas -> 0 lemmas
-- 3. use of ++ was removed (inefficiency)
-- 4. stack underflow -> arbitrary stack

