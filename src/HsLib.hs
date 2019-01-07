-- | Haskell equivalent functions of the CPP functions to test for correctness.
module HsLib
    ( square
    , isTriple
    , triples
    , applyXorCipher
    , encodeCodepoint
    , decodeToCodepoint
    )
where

import           Data.Bits                      ( xor )
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( chr
                                                , ord
                                                )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text

-- | Square a number. For example,
--
-- >>> square 9
-- 81
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

-- | List of Pythagorean triples for c < 100. Used to ensure that 20% of 
--  the random triple values generated are valid Pythagorean triples.
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

-- | Encode or decode a message with XOR and key. For example,
--
-- >>> import qualified Data.ByteString.Char8 as C
-- >>> applyXorCipher (C.pack "message") (C.pack "my key")
-- "\NUL\FSS\CAN\EOT\RS\b"
-- >>>  applyXorCipher (C.pack "\NUL\FSS\CAN\EOT\RS\b") (C.pack "my key")
-- "message"
applyXorCipher :: ByteString -> ByteString -> ByteString
applyXorCipher msg key = B.pack (B.zipWith xor msg keys)
  where
    l    = C.length msg `div` C.length key
    keys = C.concat $ replicate (l + 1) key

-- | Encode Unicode code-points to UTF-8
-- from https://rosettacode.org/wiki/UTF-8_encode_and_decode#Haskell
-- For example,
--
-- >>> encodeCodepoint 0x00F6
-- [195,182]
-- >>> encodeCodepoint 0x1D11E
-- [240,157,132,158]
encodeCodepoint :: Int -> [Int]
encodeCodepoint =
    map fromIntegral . B.unpack . Text.encodeUtf8 . Text.singleton . chr

-- | Decode UTF-8 to Unicode code-points
-- from https://rosettacode.org/wiki/UTF-8_encode_and_decode#Haskell
-- For example,
--
-- >>>  showHex  (decodeToCodepoint [195,182]) ""
-- "F6"
-- >>> showHex  (decodeToCodepoint [240,157,132,158]) ""
-- "1d11e"
decodeToCodepoint :: [Int] -> Int
decodeToCodepoint =
    ord . Text.head . Text.decodeUtf8 . B.pack . map fromIntegral
