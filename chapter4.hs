-- conditional expression

abs :: Int -> Int
abs n = if n >= 0 then n else -n

-- nesting
-- if conditions always need an else to prevent ambiguity
-- (see "dangling else" problem)

signum :: Int -> Int
signum n = if n < 0 then -1 else
              if n == 0 then 0 else 1


-- Guarded Equations

guardedAbs n | n >= 0 = n
             | otherwise = -n

guardedSignum n | n >= 0    = -1
                | n == 0    = 0
                | otherwise = 1

-- Pattern Matching

not :: Bool -> Bool
not False = True
not True  = False

-- infix operator types are defined on round brackets
(&&) :: Bool -> Bool -> Bool
True && True   = True
True && False  = False
False && True  = False
False && False = False

-- or better
-- True && True = True
-- _    && _    = False

-- in standard library, short cicuit when first arg is false
-- beautiful!
-- True  && b = b
-- False && _ = False

-- List Patterns
-- [1, 2, 3, 4] is acually 1:(2:(3:(4:[]))),
-- with colon being the "cons" operator
head :: [a] -> a
head (x:_) = x

--tail :: [a] -> [a]
--tail (_:xs) = xs

-- lambda expressions
myFunction :: Num a => [a] -> [a]
myFunction = map (*2)


-- Exercises
-- (1)
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetailGuarded :: [a] -> [a]
safetailGuarded xs | null xs = []
                   | otherwise = tail xs

safetailPattern :: [a] -> [a]
safetailPattern [] = []
safetailPattern (_:xs) = xs

-- (2)
(||) :: Bool -> Bool -> Bool

-- first and dumb
True  || True  = True
False || True  = True
True  || False = True
False || False = False

-- second and smarter
False || False = False
_     || _     = True

-- third and supersmart
False || b    = b
True  || _    = True


