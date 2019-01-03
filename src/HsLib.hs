-- | Haskell equivalent functions of the CPP functions to test for correctness.
module HsLib
    ( square
    , isTriple
    , triples
    , applyXorCipher
    )
where

import           Foreign.C.String                         ( castCharToCChar
                                                          , castCCharToChar
                                                          )
import           Data.Bits                                ( xor )

-- | Square a number.
square :: Num a => a -> a
square x = x * x

-- | Test whether a, b and c form a  Pythagorean triple given that a < b < c.
-- For example,
--
-- >>> isTriple 7 24 25
-- True
-- >>> isTriple 7 24 26
-- False
isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c

-- | List of Pythagorean triples for c < 100.
triples :: [(Int, Int, Int)]
triples =
    [ (3 , 4 , 5)
    , (5 , 12, 13)
    , (8 , 15, 17)
    , (7 , 24, 25)
    , (20, 21, 29)
    , (12, 35, 37)
    , (9 , 40, 41)
    , (28, 45, 53)
    , (11, 60, 61)
    , (33, 56, 65)
    , (16, 63, 65)
    , (48, 55, 73)
    , (36, 77, 85)
    , (13, 84, 85)
    , (39, 80, 89)
    , (65, 72, 97)
    ]

applyXorCipher :: String -> String -> String
applyXorCipher msg key = map castCCharToChar xs
  where
    msg' = map castCharToCChar msg
    key' = map castCharToCChar key
    xs   = zipWith xor msg' (concat [ key' | r <- [0 ..] ])

