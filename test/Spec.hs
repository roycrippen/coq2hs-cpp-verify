module Main where

import qualified HsLib                         as HS
import qualified InlineCPP                     as CPP
import           Control.Monad                  ( (>=>) )
import           Foreign.C.Types                ( CInt )
import qualified Data.ByteString.Char8         as C
import qualified Test.QuickCheck.Unicode       as QCU
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import qualified Test.QuickCheck.Monadic       as QCM

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
        it "quickcheck: HS.decodeToCodepoint (HS.encodeCodepoint cp) == cp"
            $ quickCheck (withMaxSuccess 10000 prop_hsEncodeDecodeCodepoint)
    describe "\nCPP Functions" $ do
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
    describe "\nHaskell checking CPP, HS.f args == CPP.f args" $ do
        it "quickcheck: HS.square a == CPP.square a" $ quickCheck prop_square
        it "quickcheck: HS.isTriple a b c == CPP.isTriple a b c"
            $ quickCheck prop_pTriples
        it "quickcheck: HS.applyXorCipher s k == CPP.applyXorCipher s k"
            $ quickCheck (withMaxSuccess 10000 prop_applyXorCipher)
        it "quickcheck: CPP.decodeToCodepoint(HS.encodeCodepoint cp) == cp"
            $ quickCheck (withMaxSuccess 10000 prop_hsEncodeCppDecodeCodepoint)
        it "quickcheck: HS.decodeToCodepoint(CPP.encodeCodepoint cp) == cp"
            $ quickCheck (withMaxSuccess 10000 prop_cppEncodeHSDecodeCodepoint)


unicodeVal :: Gen Int
unicodeVal = excluding QCU.reserved (frequency QCU.planes)

excluding :: (a -> Bool) -> Gen a -> Gen a
excluding bad gen = loop
  where
    loop = do
        x <- gen
        if bad x then loop else return x

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

prop_square :: Property
prop_square = forAll arbitrary $ \n -> QCM.monadicIO $ do
    nn <- QCM.run $ CPP.square (n :: CInt)
    QCM.assert $ nn == HS.square n

prop_pTriples :: Property
-- prop_pTriples = forAll pTripleVals $ \(a, b, c) ->
--     collect (a, b, c) $ monadicIO $ do
prop_pTriples = forAll pTripleVals $ \(a, b, c) -> QCM.monadicIO $ do
    let a' = fromIntegral a
        b' = fromIntegral b
        c' = fromIntegral c
    i <- QCM.run (CPP.isTriple a' b' c')
    QCM.assert (HS.isTriple a b c == (i == 1))

prop_cppApplyXorCipher :: Property
prop_cppApplyXorCipher = forAll arbitrary $ \s0 -> QCM.monadicIO $ do
    let key = C.pack "some string key 1234"
    s1 <- QCM.run $ CPP.applyXorCipher s0 key
    s2 <- QCM.run $ CPP.applyXorCipher s1 key
    QCM.assert $ s2 == s0

prop_hsApplyXorCipher :: Property
prop_hsApplyXorCipher = forAll arbitrary $ \s -> QCM.monadicIO $ do
    let key = C.pack "some string key 1234"
    QCM.assert $ HS.applyXorCipher (HS.applyXorCipher s key) key == s

prop_applyXorCipher :: Property
prop_applyXorCipher = forAll arbitrary $ \s -> QCM.monadicIO $ do
    let key = C.pack "some string key 1234"
    s1 <- QCM.run $ CPP.applyXorCipher s key
    QCM.assert $ s1 == HS.applyXorCipher s key

prop_hsEncodeDecodeCodepoint :: Property
prop_hsEncodeDecodeCodepoint = forAll unicodeVal $ \cp ->
    QCM.monadicIO
        $  QCM.assert
        $  HS.decodeToCodepoint (HS.encodeCodepoint cp)
        == cp

prop_hsEncodeCppDecodeCodepoint :: Property
prop_hsEncodeCppDecodeCodepoint = forAll unicodeVal $ \cp -> QCM.monadicIO $ do
    let values = HS.encodeCodepoint cp
    cp' <- QCM.run $ CPP.decodeToCodepoint values
    QCM.assert $ cp' == cp

prop_cppEncodeHSDecodeCodepoint :: Property
prop_cppEncodeHSDecodeCodepoint = forAll unicodeVal $ \cp -> QCM.monadicIO $ do
    values <- QCM.run $ CPP.encodeCodepoint cp
    let cp' = HS.decodeToCodepoint values
    QCM.assert $ cp' == cp

