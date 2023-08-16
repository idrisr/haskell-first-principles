module Funcs where

import Control.Applicative

j :: Monad m => m (m a) -> m a
{- hlint ignore "Use join" -}
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

yo :: Monad m => m a -> m (a -> b) -> m b
yo = flip (<*>)

sequ :: Monad m => [m b] -> m [b]
sequ = foldr f b
  where
    f = liftA2 (:)
    b = pure []

-- You’ll need recursion for this one:
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = sequ $ fmap f xs

-- 6. Hint: reuse meh:
flipType :: Monad m => [m a] -> m [a]
flipType = sequ
