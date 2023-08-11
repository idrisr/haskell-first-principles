import Test.QuickCheck

newtype T a = T a
f1 :: a -> b a -> T (b a)
f1 = undefined

data FixmePls a =
    FixMe
    | Pls deriving (Eq, Show)

instance Functor FixmePls where
    fmap _ FixMe = FixMe
    fmap _ Pls = Pls

_ = fmap (+1) FixMe

tO = fmap (+1) negate

n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms = [ave, n, w]

replaceWithP :: b -> Char
replaceWithP = const 'p'
az = fmap             replaceWithP lms
bz = (fmap.fmap)      replaceWithP lms
cz = (fmap.fmap.fmap) replaceWithP lms

a = (+1) <$> read "[1]"::[Int]
b = fmap (++ "lol") <$> Just ["Hi,", "Hello"]
c = (*2) <$> (\x -> x - 2)
d = (return '1' ++) . show <$> (\x -> [x, 1..3])

f :: Integer
f = 1
g :: String
g = show f
h :: Integer
h = read $ "123" ++ g

fi :: IO Integer
fi = readIO "1"
gi :: IO String
gi = fmap show fi
hi :: IO String
hi = ("123"++) <$> gi
ii :: IO Int
ii = read <$> hi

ai:: IO Integer
ai = read.("123"++).show <$> fi

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read.("123"++).show <$> ioi
    in (*3) <$> changed

{- HLINT ignore "Functor law" -}
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                            (a->b)
                         -> (b->c)
                         -> f a
                         -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g.f) x

f2 :: [Int] -> Bool
f2 = functorIdentity

c2 :: [Int] -> Bool
c2 = functorCompose (+1) (*2)

li :: [Int] -> Bool
li x = c2 (x::[Int])
lj x = functorCompose (+1) (*2) (x::(Identity Int))

newtype Identity a = Identity a deriving (Eq, Arbitrary, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

f3 :: Identity Int -> Bool
f3 = functorIdentity
f4 :: Identity String -> Bool
f4 = functorIdentity

main = do
    quickCheck f4
    quickCheck f3
    quickCheck c2
    quickCheck $ functorCompose (*8) (+4) (Identity (9::Int))
