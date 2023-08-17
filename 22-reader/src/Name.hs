module Name where

newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String
    deriving (Eq, Show)

data Person = Person
    { humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    }
    deriving (Eq, Show)

data Dog = Dog
    { dogsName :: DogName
    , dogsAddress :: Address
    }
    deriving (Eq, Show)

pers :: Person
pers =
    Person
        (HumanName "Big Bird")
        (DogName "Barkley")
        (Address "Sesame Street")

chris :: Person
chris =
    Person
        (HumanName "Chris Allen")
        (DogName "Papu")
        (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
    d <- dogName
    a <- address
    return $ Dog d a

getDogRM2 :: Person -> Dog
getDogRM2 = dogName >>= \d -> address >>= \a -> return $ Dog d a

myLiftA2 :: Applicative f => (a->b->c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b
