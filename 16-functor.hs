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
h = read $ ("123"++) g

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
