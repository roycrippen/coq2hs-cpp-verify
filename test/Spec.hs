module Main where

import qualified InlineCPP                     as CPP
import qualified HsLib                         as HS
import           Test.Hspec                               ( shouldBe
                                                          , describe
                                                          , hspec
                                                          , it
                                                          )
import           Test.QuickCheck                          ( Arbitrary
                                                          , Gen
                                                          , Property(..)
                                                          , arbitrary
                                                          , choose
                                                          , elements
                                                          , forAll
                                                          , frequency
                                                          , listOf
                                                          , sized
                                                          , suchThat
                                                          , quickCheck
                                                          , withMaxSuccess
                                                          )
import           Test.QuickCheck.Monadic                  ( monadicIO
                                                          , run
                                                          , assert
                                                          )
import           Foreign.C.Types
import           Control.Monad                            ( (>=>) )

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
        it "quickcheck: HS.applyXorCipher (HS.applyXorCipher s k) k == s"
            $ quickCheck (withMaxSuccess 10000 prop_hsApplyXorCipher)
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
        it "quickcheck: CPP.applyXorCipher (CPP.applyXorCipher s k) k == s"
            $ quickCheck (withMaxSuccess 10000 prop_cppApplyXorCipher)
    describe "Haskell checking CPP, HS.f args == CPP.f args" $ do
        it "quickcheck: HS.isTriple a b c == CPP.isTriple a b c"
            $ quickCheck prop_pTriples
        it "quickcheck: HS.applyXorCipher s k == CPP.applyXorCipher s k"
            $ quickCheck (withMaxSuccess 10000 prop_applyXorCipher)

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

prop_cppApplyXorCipher :: Property
prop_cppApplyXorCipher = forAll genSafeString $ \s0 -> monadicIO $ do
    let key = "some string key 1234"
    s1 <- run $ CPP.applyXorCipher s0 key
    s2 <- run $ CPP.applyXorCipher s1 key
    assert $ s2 == s0

prop_hsApplyXorCipher :: Property
prop_hsApplyXorCipher = forAll genSafeString $ \s -> monadicIO $ do
    let key = "some string key 1234"
    assert $ HS.applyXorCipher (HS.applyXorCipher s key) key == s

prop_applyXorCipher :: Property
prop_applyXorCipher = forAll genSafeString $ \s -> monadicIO $ do
    let key = "some string key 1234"
    s1 <- run $ CPP.applyXorCipher s key
    assert $ s1 == HS.applyXorCipher s key

genSafeChar :: Gen Char
genSafeChar = elements [' ' .. '~']

genSafeString :: Gen String
genSafeString = listOf genSafeChar
