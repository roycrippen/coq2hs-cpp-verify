{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InlineCPP
  ( square
  , isTriple
  , testApplyXorCipher
  , encode
  , decode
  , sumVec
  , rangeList
  , applyXorCipher
  )
where

import           Data.Monoid                              ( (<>) )
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
applyXorCipher :: String -> IO String
applyXorCipher cs = do
  -- create c-string to pass to cpp 
  let n = length cs
  let nCInt = fromIntegral n :: CInt
  (cStr, n_) <- newCStringLen cs
  -- putStrLn $ "n = " ++ show n ++ ", n_ = " ++ show n_

  -- cpp code to decode list of ints
  cStr <- [C.block| char* {
      // rename input pointer to a more readable variable
      std::string s = (std::string) $(char *cStr);
      int n = $(int nCInt);

      string key = "cipher key 123";
      auto ds = cn::applyXorCipher(s, key);

      // convert decoded std::string to a char* and return it
      char *cstr = new char[n + 1];
      std::strcpy(cstr, ds.c_str());
      return cstr;
    }|]

  -- cout << "s = '" << s << "', ds = '" << ds << "', n = " << n << "\n";
  -- extract CString into an IO String and cleanup allocated memory
  res <- peekCStringLen (cStr, n)
  free cStr
  putStrLn $ "cs = " ++ show cs ++ ", res = " ++ show res
  return res


-- encode string to a list of ints by calling cn::encode(...) cpp function
-- brittany-disable-next-binding
encode :: String -> IO [CInt]
encode cs = do
  -- create c-string to pass to cpp 
  let n = length cs
  (cStr, n_) <- newCStringLen cs
  -- putStrLn $ "n = " ++ show n ++ ", n_ = " ++ show n_

  -- cpp code encode the c-string to a array of ints 
  arrayPtr <- [C.block| int* {
      std::string s = (std::string) $(char *cStr); 
      auto encoded_vec = cn::encode(s);

      // int* arr = encoded_vec.data(); // did not work, first two values wrong
      // int* arr = &encoded_vec[0];    // did not work, first two values wrong

      int len = encoded_vec.size();
      int* arr = new int[len];
      for (int i = 0; i < len; i++) {
        arr[i] = encoded_vec[i];
      }

      return arr;
    } |]

  -- free allocated mewmory and return the list of ints
  free cStr
  peekArray n arrayPtr

-- decode list of ints to a String by calling cn::decode(...) cpp function
-- brittany-disable-next-binding
decode :: [CInt] -> IO String
decode xs = do
  -- create mutable vector that cpp block can reference
  vec <- V.thaw (V.fromList xs)

  -- cpp code to decode list of ints
  cStr <- [C.block| char* {
      // rename input pointer to a more readable variable
      int* xs = $vec-ptr:(int *vec);
      int len = $vec-len:vec;

      // convert input pointer to a vector of int and decode
      std::vector<int> encoded_vec;
      for (int i = 0; i < len; i++) {
        encoded_vec.push_back(xs[i]);
      }
      auto ds = cn::decode(encoded_vec);

      // convert decoded std::string to a char* and return it
      char *cstr = new char[ds.length() + 1];
      std::strcpy(cstr, ds.c_str());
      return cstr;
    }|]

  -- extract CString into an IO String and cleanup allocated memory
  res <- peekCString cStr
  _   <- V.freeze vec
  free cStr
  return res

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
