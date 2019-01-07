{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell wrapper functions for using CPP directly as inline source code.
module InlineCPP
  ( square
  , isTriple
  , sumVec
  , rangeList
  , testApplyXorCipher
  , applyXorCipher
  , encodeCodepoint
  , decodeToCodepoint
  )
where

import           Data.ByteString                ( ByteString )
import           Data.Monoid                    ( (<>) )
import           Foreign.C.Types                ( CInt
                                                , CUChar
                                                )
import           Foreign.Marshal.Alloc          ( free )
import           Foreign.Marshal.Array          ( peekArray )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Unsafe        as BU
import qualified Data.Vector.Storable          as V
import qualified Data.Vector.Storable.Mutable  as VM
import qualified Language.C.Inline.Cpp         as C
import qualified HsLib                         as HS

C.context (C.cppCtx <> C.vecCtx)
C.include "<iostream>"
C.include "<cstring>"
C.include "../cpp/include/candidates.hpp"

-- brittany-disable-next-binding
-- | Inline call to CPP function cn::square.
--  Equivalent to HS function 'HS.square'. For example,
--
-- >>> square 9
-- 81
square :: CInt -> IO CInt
square x = [C.exp| int { cn::square($(int x)) } |]

-- brittany-disable-next-binding
-- | Inline call to CPP function cn::isTriple. 
--  Equivalent to HS function 'HS.isTriple'. For example,
--
-- >>> isTriple 7 24 25
-- True
-- >>> isTriple 7 24 26
-- False
isTriple :: CInt -> CInt -> CInt -> IO CInt
isTriple a b c =
   [C.exp| int { cn::is_pythagorean_triple($(int a), $(int b), $(int c)) } |]

-- brittany-disable-next-binding
-- | Inline CPP that sums items in a HS list. For example,
--
-- >>> sumVec [10,20,30]
-- 100
sumVec :: [CInt] -> IO CInt
sumVec xs = do
  -- create mutable vector that cpp block can reference
  vec <- V.thaw (V.fromList xs)

  -- cpp code to sum the list 
  n <- [C.block| int {
    int* xs = $vec-ptr:(int *vec);
    int len = $vec-len:vec;

    int sum = 0;
    for (int i = 0; i < $vec-len:vec; i++) {
        sum += xs[i];
    }
    return sum;
  } |]

  -- return the sum
  _ <- V.freeze vec
  return n

-- brittany-disable-next-binding
-- | Inline CPP that creates a HS list containing [0 .. (n - 1)]. For example,
--
-- >>> rangeList 4
-- [0,1,2,3]
rangeList :: Int -> IO [Int]
rangeList _n = do
  -- cast _n from Int to CInt
  let n = fromIntegral _n

  -- build a list in a cpp block
  arrayPtr <- [C.block| int* {
    int n_ = $(int n);
    int* mat;
    mat = new int[n_];
    for (int i = 0; i < n_; i++) {
      mat[i] = i;
    }
    return mat;
  } |]

  -- copy ptr contents to list of Int
  ls <- peekArray _n arrayPtr
  free arrayPtr
  return $ map fromIntegral ls


-- brittany-disable-next-binding
-- | Inline CPP calls cn::applyXorCipher twice to encoded then decode a message.
-- Demonstrates use of cout and successful round trip use of the XOR cipher.
-- For example,
-- 
-- >>> testApplyXorCipher
-- This is message from cpp std::cout.
-- std::cout -> (cs, key): ('This is the test string...', 'cipher key 123')'
-- std::cout -> applyXorCipher(applyXorCipher(cs, key), key): 'This is the test string...'
testApplyXorCipher :: IO ()
testApplyXorCipher = [C.exp| void {
    cout << "This is message from cpp std::cout.\n" ;
    auto cs = "This is the test string...";
    string key = "cipher key 123";
    cout << "std::cout -> (cs, key): ('" << cs << "', '" << key << "')" << "'\n";
    auto orig_str = cn::applyXorCipher(cn::applyXorCipher(cs, key), key);
    cout << "std::cout -> applyXorCipher(applyXorCipher(cs, key), key): '" << orig_str << "'\n";
  }|]

-- brittany-disable-next-binding
-- | Inline call to CPP function cn::applyXorCipher. Equivalent to HS function 'HS.applyXorCipher'.
-- Used to encode or decode a message with XOR and key. For example,
--
-- >>> import qualified Data.ByteString.Char8 as C
-- >>> applyXorCipher (C.pack "message") (C.pack "my key")
-- "\NUL\FSS\CAN\EOT\RS\b"
-- >>>  applyXorCipher (C.pack "\NUL\FSS\CAN\EOT\RS\b") (C.pack "my key")
-- "message"
applyXorCipher :: ByteString -> ByteString -> IO ByteString
applyXorCipher msg key =  do
  (res, size) <- BU.unsafeUseAsCStringLen msg $ \(cStrIn, cStrInLength) ->
    BU.unsafeUseAsCString key $ \cStrKey -> do
      let nCInt = fromIntegral cStrInLength :: CInt
      cStrOut <- [C.block| char* {
        // rename inputs to more readable names
        char *cStrIn = $(char *cStrIn);
        char *key = $(char *cStrKey);
        int n = $(int nCInt);

        // copy *char to string (not casting)
        // necessary because string might contain NULL
        std::string s = "";
        for (int i = 0; i < n; i++) {
          s.push_back(cStrIn[i]);
        }

        auto ds = cn::applyXorCipher(s, key);

        // convert decoded std::string to a char* and return it
        char *cstr = new char[n + 1];
        std::memcpy(cstr, ds.data(), n);
        return cstr;
      }|]
      return (cStrOut, cStrInLength)

  B.packCStringLen (res, size)


-- brittany-disable-next-binding
encodeCodepoint :: Int -> IO [Int]
encodeCodepoint _n = do
  inVec <- V.thaw (V.fromList ([-1,-1,-1,-1]::[CInt]))
  let n = fromIntegral _n :: CInt

  [C.block| void {
    int* res = $vec-ptr:(int *inVec);
    auto i = static_cast<uint32_t>($(int n));

    vector<uint8_t> es = cn::encodeCodepoint(i);

    for (int i = 0; i < es.size(); i++) {
      res[i] = es[i];
    }
    } |]

  outVec <- V.freeze inVec
  let res = map fromIntegral (V.toList outVec)
  return $ filter (/= -1) res

-- brittany-disable-next-binding
decodeToCodepoint :: [Int] -> IO Int
decodeToCodepoint xs = do
  inVec <- V.thaw (V.fromList $ map fromIntegral xs :: V.Vector CInt)
  let len = fromIntegral (length xs) :: CInt

  fromIntegral <$> [C.block| int {
    int* xs = $vec-ptr:(int *inVec);
    int len = $(int len);

    vector<uint8_t> us = {};
    for (int i = 0; i < len; i++) {
      us.push_back(xs[i]);
    }

    auto res = cn::decodeToCodepoint(us);

    return res;
    } |]
