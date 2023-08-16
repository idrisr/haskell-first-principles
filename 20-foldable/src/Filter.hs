module Filter where

-- Write a filter function for Foldable types using the foldMap function:
filterF ::
    (Applicative f, Foldable t, Monoid (f a)) =>
    (a -> Bool) ->
    t a ->
    f a
filterF f =
    let g a =
            if f a
                then pure a
                else mempty
     in foldMap g
