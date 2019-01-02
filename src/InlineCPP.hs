{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InlineCPP
  ( square
  , isTriple
  , testApplyXorCipher
  , sumVec
  , rangeList
  , applyXorCipher
  )
where

import           Data.Monoid                    ( (<>) )
import           Control.Monad
import qualified Data.Vector.Storable          as V
import qualified Data.Vector.Storable.Mutable  as VM
import           Foreign.C.Types
import           Foreign.C.String
import qualified Language.C.Inline.Cpp         as C
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc

C.context (C.cppCtx <> C.vecCtx)
C.include "<iostream>"
C.include "<cstring>"
C.include "../cpp/include/candidates.hpp"

-- brittany-disable-next-binding
square :: CInt -> IO CInt
square x = [C.exp| int { cn::square($(int x)) } |]

-- brittany-disable-next-binding
isTriple :: CInt -> CInt -> CInt -> IO CInt
isTriple a b c =
   [C.exp| int { cn::is_pythagorean_triple($(int a), $(int b), $(int c)) } |]

-- brittany-disable-next-binding
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
applyXorCipher :: String -> String -> IO String
applyXorCipher cs key = do
  -- create c-string to pass to cpp 
  let nCInt = fromIntegral (length cs) :: CInt
  cStrIn <- newCString cs
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
  res <- peekCStringLen (cStrOut, length cs)
  free cStrIn
  free cStrOut
  -- putStrLn $ "cs = " ++ show ( map fromEnum cs) ++
  --          ", res = " ++ show ( map fromEnum res)
  return res


toIntList :: String -> [Int]
toIntList = map fromEnum

-- sum elements of a list
-- brittany-disable-next-binding
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

-- create a list with cpp
-- brittany-disable-next-binding
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
