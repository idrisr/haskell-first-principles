import MyLib
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (head, minimum)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (>= x) xs

trivialInt :: Gen Int
trivialInt = return 1

oneThruThree :: Gen Int
oneThruThree = elements [1 .. 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Left a, Left a, Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Just a, Nothing]

prop_isTrue :: Int -> Bool
prop_isTrue _ = True

prop_isTrub :: Int -> Int -> Bool
prop_isTrub _ _ = False

newtype Identity a = Identity a deriving (Eq, Show)
identityGen :: Arbitrary a => Gen (Identity a)
{- HLINT ignore "Use <$>" -}
identityGen = do
    a <- arbitrary
    return (Identity a)

prop_minimum :: Ord a => [a] -> Bool
prop_minimum xs = head (qsort xs) == minimum xs

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

-- prop_minimum' :: Ord a => [a] -> Bool
-- prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5 :: Integer, 0)
        it
            "22 divided by 5 is\
            \ 4 remainder 2"
            $ do
                dividedBy 22 5 `shouldBe` (4 :: Integer, 2)
        it
            "x + 1 is always\
            \ greater than x"
            $ do
                property $ \x -> x + 1 > (x :: Int)
