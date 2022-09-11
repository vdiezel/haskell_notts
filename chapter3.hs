-- currying!

add :: (Int, Int) -> Int
add (x,y) = x + y

add' :: Int -> (Int -> Int)
add' x y = x + y -- this is curried!

-- Convention to prevent excess parentheses on type def for curried functions

-- instead of Int -> (Int -> (Int -> Int))
-- write: Int -> Int -> Int -> Int
-- as the -> arrow assosicates to the right
-- the consequence is that function application associates to the left
-- mult x y z means ((mult x) y) z


-- polymorphic: when its type contains one of more type variables
-- length :: [a] -> Int

-- overloaded: w polymorphic function that contains one or more class contraints

--Exercises
-- (1)
-- [Char],
-- (Char, Char, Char),
-- [(Bool, Char)],
-- ([Bool], [Char]),
-- [[a] -> [a]]

-- (2)
-- second :: [a] -> a
-- swap :: (a, b) -> (b, a)
-- pair :: a -> b -> (a, b)
-- double :: Num a => a -> a
-- paldindrome :: Eq a => [a] -> Bool
-- twice :: (a -> a) -> a -> a
