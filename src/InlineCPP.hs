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
  )
where

import           Data.Monoid                              ( (<>) )
import qualified Data.Vector.Storable          as V
import qualified Data.Vector.Storable.Mutable  as VM
import           Foreign.C.Types                          ( CInt )
import           Foreign.C.String                         ( newCString
                                                          , peekCStringLen
                                                          )
import qualified Language.C.Inline.Cpp         as C
import           Foreign.Marshal.Array                    ( peekArray )
import           Foreign.Marshal.Alloc                    ( free )
import qualified HsLib                         as HS

C.context (C.cppCtx <> C.vecCtx)
C.include "<iostream>"
C.include "<cstring>"
C.include "../cpp/include/candidates.hpp"

-- brittany-disable-next-binding
-- | Inline call to CPP function cn::square.
--  Equivalent to HS function 'HS.square'
square :: CInt -> IO CInt
square x = [C.exp| int { cn::square($(int x)) } |]

-- brittany-disable-next-binding
-- | Inline call to CPP function cn::isTriple. 
--  Equivalent to HS function 'HS.isTriple'.
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
testApplyXorCipher :: IO ()
testApplyXorCipher = [C.exp| void {
    cout << "This is message from cpp std::cout.\n" ;
    auto cs = "This is the test string...";
    string key = "cipher key 123";
    cout << "std::cout -> (cs, key): ('" << cs << "', '" << key << "')" << "'\n";
    auto orig_str = cn::applyXorCipher(cn::applyXorCipher(cs, key), key);
    cout << "std::cout -> applyXorCipher(applyXorCipher(cs, key), key): '" << orig_str << "'\n";
  }|]

-- encode string to a list of ints by calling cn::encode(...) cpp function
-- brittany-disable-next-binding
-- | Inline call to CPP function cn::applyXorCipher. Equivalent to HS function 'HS.applyXorCipher'.
-- Used to encode or decode a message with XOR and key. For example,
--
-- >>> applyXorCipher "message" "my key"
-- "\NUL\FSS\CAN\EOT\RS\b"
-- >>>  applyXorCipher "\NUL\FSS\CAN\EOT\RS\b" "my key"
-- "message"
applyXorCipher :: String -> String -> IO String
applyXorCipher msg key = do
  -- create c-string to pass to cpp 
  let nCInt = fromIntegral (length msg) :: CInt
  cStrIn <- newCString msg
  cStrKey <- newCString key

  -- cpp code to call cn::applyXorCipher(string, key)
  cStrOut <- [C.block| char* {
      // rename input pointer and n to a more readable names
      char *cStrIn = $(char *cStrIn);
      char *key = $(char *cStrKey);
      int n = $(int nCInt);

      // copy *char to string 
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

  -- get the result string out of the pointer, cleanup memory and return
  res <- peekCStringLen (cStrOut, length msg)
  free cStrIn
  free cStrOut
  -- putStrLn $ "cs = " ++ show ( map fromEnum cs) ++
  --          ", res = " ++ show ( map fromEnum res)
  return res

