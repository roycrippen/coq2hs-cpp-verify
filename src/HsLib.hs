module HsLib
    ( square
    , isTriple
    , triples
    )
where

square :: Int -> Int
square x = x * x

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c

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
