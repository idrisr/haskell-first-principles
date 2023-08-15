module Novel where

import Control.Monad (join)

andOne :: Num a => a -> [a]
andOne x = [x, 1]

xs :: [Integer]
{- hlint ignore "Use concatMap" -}
xs = concat $ fmap andOne [4..6]

bind :: Monad m => (a -> m b) -> m a -> m b
{- hlint ignore "Use =<<" -}
bind f a = join $ fmap f a
