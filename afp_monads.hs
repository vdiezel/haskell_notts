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

evalMon :: Expr -> Maybe Int
evalMon (Val n) = Just n
evalMon (Div x y) = evalMon x >>= (\n -> evalMon y >>= (\m -> safediv n m))

-- can also be done using the do notation
evalDo :: Expr -> Maybe Int
evalDo (Val n) = Just n
evalDo (Div x y) = do n <- eval x
                      m <- eval y
                      safediv n m
