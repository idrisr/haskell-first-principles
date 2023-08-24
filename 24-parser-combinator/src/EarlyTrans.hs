module EarlyTrans where

import Control.Monad.Trans.State
import Data.Monoid

rs :: StateT s m a -> s -> m (a, s)
rs = runStateT

a :: IO ((), Sum Integer)
a = runStateT (put 3) mempty

b :: IO (Product Integer , Product Integer)
b = runStateT (put 1 >> get) mempty

c :: IO (Sum Integer, Sum Integer)
c = (runStateT $ put 1 >> get) mempty

d :: IO (Sum Integer, Sum Integer)
d = (rs $ put 2 >> get) mempty

e :: IO (Sum Integer, Product Integer)
e = (rs $ put 2 >> return 9001) mempty
