-- LESSON 1:

-- a simple evaluator

data Expr = Val Int | Div Expr Expr

-- e :: Expr
-- e = Div (Val 6) (Val 3)

eval' :: Expr -> Int
eval' (Val n) = n
eval' (Div x y) = eval' x `div` eval' y

-- Problem: Not Safe
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                   Nothing -> Nothing
                   Just n -> case eval y of
                     Nothing -> Nothing
                     Just m -> safediv n m

evalApp :: Expr -> Maybe Int
evalApp (Val n) = pure n
evalApp (Div x y) = pure safediv (*) evalApp x <*> evalApp y -- type error! because savediv is NOT pure
-- there is no way for us to match the required type Int -> Int -> Maybe Int

-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b -- (>>=) is the bind operator
-- Nothing >>= f = Nothing
-- (Just x) >>= f = f x

-- evalMon :: Expr -> Maybe Int
-- evalMon (Val n) = Just n
-- evalMon (Div x y) = evalMon x >>= (\n -> evalMon y >>= (\m -> safediv n m))

-- can also be done using the do notation
evalDo :: Expr -> Maybe Int
evalDo (Val n) = Just n
evalDo (Div x y) = do n <- eval x --syntactic sugar
                      m <- eval y
                      safediv n m

-- m1 >= \x1 -> m2 >>= \x2 >>= ... mn >>= \xn -> f x1 x2 .. xn

-- LESSON 2:

-- formal definition of a monad in Haskell: Applicative that support a bind operator

class Applicative m => MyMonad m where
  -- return :: a -> ma
  return = pure
  -- (>>=) :: m a -> (a -> m b) -> m b

-- | example: Maybe
instance MyMonad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= f = Nothing
  Just x >>= f = f x

-- | Example: List

instance MyMonad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  --xs >>= f = concat (map f xs) -- or ...
  xs >>= f = [y | x <- xs, y <- fx]

-- | Example: State

type State = Int
type StateTransformer a = State -> (a, State)

-- reduces the constructor overhead
newtype ST a = S(State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) s = st s

-- we have to make ST an instance of Functor and an Applicative
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Monad ST where
  -- return :: a -> ST a
  return x = S (\s -> (x,s))
  -- >>= :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
                  let (x, s') = app st s
                  in app (f x s'))

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf x)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

label :: Tree a -> Tree Int
label tree = fst (app (mlabel tree) 0)


-- LESSON 4
-- generalized map to work for an arbotrary monad

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []     = return []
mapM f (x:xs) = do y <- f x
                   ys <- Main.mapM f xs
                   return (y:xs)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

-- mapM conv "1234" -> Just [1, 2, 3, 4]

join:: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x

-- Monad Laws
-- 1. Link between "return" and "bind" functions
-- return x >>= f = f x
-- 2. "bind" on the righ hand side
-- mx >>= return =  m x ("return" is somewhat the identity of the bind operator)
-- 3. Links the bind operator with itself (associativity)
-- (mx >> f) >>= g == mx >>= (f >>= g) is wrong! typerror

-- (mx >> f) >>= g == mx >>= (\x -> (f x >>= g))

-- Effectful Programming - wrap up

-- a -> b vs a -> m b

-- Final Piece

data Expr a = Var a
  | Val Int
  | Add (Expr a) (Expr a)

instance Monad Expr where
  return x = Var x
  (Var v)   >>= f = f v
  (Val n)   >>= f = Val n
  (Add x y) >>= f = Add (x >== f) (y >== f)

f :: Char -> Expr

f 'x' = Add (Val 1) (Val 2)
f 'y' = Val 3












