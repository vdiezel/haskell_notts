-- Generalising fmap

fmap0 :: a -> f a
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

-- How to have a general way of expressing arbitrary amount of parameters?
-- two basic functions
pure  :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b -- generalised form of function applications

-- pure g <*> x <*> y <*> z -- applicative style (brackets to the left)

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y
-- (a -> b -> c) f a -> f b = f(a -> b) -> f a -> f b -> f c
--
--
-- An applicative functors is a functors thats support the pure and the star operator

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Maybe applicative

instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure x = Just x
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> mx   = Nothing
  (Just g) <*> mx  = fmap g mx

-- haven't fully understood that yet!
