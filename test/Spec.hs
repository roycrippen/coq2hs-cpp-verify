module Main where

import qualified InlineCPP                     as CPP
import qualified HsLib                         as HS
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Foreign.C.Types
import           Control.Monad

main :: IO ()
main = hspec $ do
    describe "HS functions" $ do
        it "square a negative integer" $ do
            HS.square (-3) `shouldBe` 9
            HS.square (-0) `shouldBe` 0
            HS.square (-81) `shouldBe` HS.square 81
        it "square positive integer" $ do
            HS.square (HS.square 2) `shouldBe` 16
            HS.square 9 `shouldBe` 81
        it "(3,4,5) is     a triplet" $ HS.isTriple 3 4 5 `shouldBe` True
        it "(3,4,6) is NOT a triplet" $ HS.isTriple 3 4 6 `shouldBe` False
    describe "CPP Functions" $ do
        it "sqaure a negative integer" $ do
            CPP.square (-3) >>= \v -> v `shouldBe` 9
            CPP.square (-0) >>= \v -> v `shouldBe` 0
            v1 <- CPP.square 81
            v2 <- CPP.square (-81)
            v1 `shouldBe` v2
        it "square positive integer" $ do
            CPP.square 2 >>= (CPP.square >=> (`shouldBe` 16))
            CPP.square 9 >>= \v -> v `shouldBe` 81
        it "(3,4,5) is     a triplet" $ CPP.isTriple 3 4 5 >>= \v ->
            v `shouldBe` 1
        it "(3,4,6) is not a triplet" $ CPP.isTriple 3 4 6 >>= \v ->
            v `shouldBe` 0
        it "encode 'abc 123'" $ CPP.encode "abc 123" >>= \v ->
            v `shouldBe` [97, 98, 99, 32, 49, 50, 51]
        it "decode [97, 98, 99, 32, 49, 50, 51]"
            $   CPP.decode [97, 98, 99, 32, 49, 50, 51]
            >>= \v -> v `shouldBe` "abc 123"
        it "decode (encode 'abc 123')" $ do
            v <- CPP.decode =<< CPP.encode "abc 123"
            v `shouldBe` "abc 123"
        it "decode (encode ...)" $ quickCheck prop_cppEncodeDecode
    describe "Haskell checking CPP, HS_f() == CPP_f()" $ do
        it "pythagorean triplets" $ quickCheck prop_pTriples
        it "pythagorean triplets" $ quickCheck prop_pTriples

flexList :: Arbitrary a => Gen [a]
flexList = sized
    $ \n -> frequency [(1, return []), (n, (:) <$> arbitrary <*> flexList)]

rangeVal :: Gen Int
rangeVal = choose (3, 1000)

rangeVals :: Gen (Int, Int, Int)
rangeVals = do
    v1 <- rangeVal
    v2 <- rangeVal
    v3 <- rangeVal
    frequency [(1, elements HS.triples), (5, return (v1, v2, v3))]

pTripleVals :: Gen (Int, Int, Int)
pTripleVals = do
    (v1, v2, v3) <- rangeVals `suchThat` (\(v1, v2, v3) -> v1 < v2 && v2 < v3)
    return (v1, v2, v3)

prop_pTriples :: Property
-- prop_pTriples = forAll pTripleVals $ \(a, b, c) ->
--     collect (a, b, c) $ monadicIO $ do
prop_pTriples = forAll pTripleVals $ \(a, b, c) -> monadicIO $ do
    let a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
    i <- run (CPP.isTriple a' b' c')
    assert (HS.isTriple a b c == (i == 1))

prop_cppEncodeDecode = forAll genSafeString $ \s -> monadicIO $ do
    v1 <- run $ CPP.encode s
    v2 <- run $ CPP.decode v1
    assert $ v2 == s

genSafeChar :: Gen Char
genSafeChar = elements [' ' .. '~']

genSafeString :: Gen String
genSafeString = listOf genSafeChar
