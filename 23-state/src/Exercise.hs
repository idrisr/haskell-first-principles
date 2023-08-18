module Exercise where

import Moi

-- Construct a State where the state is also the value you return:
get :: Moi s s
get = Moi $ \s -> (s, s)

-- construct a state where the resulting state is the argument
-- provided, and the value defaults to unit:
put :: s -> Moi s ()
put s = Moi $ const ((), s)

-- run the state with s and get the value that results:
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
