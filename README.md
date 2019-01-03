# coq2hs-cpp-verify
Proof of concept test bench to code compare functions implemented in Haskell and C++.

requires haskell [stack](https://docs.haskellstack.org/en/stable/README/) >= 1.9.8 and (gcc >= 5.0 or clang >= 3.8)  

## Quick Start

build
```bash
stack build
```

run sample application calling both Haskell and C++ functions
```bash
stack run
```

run property based tests and unit tests comparing results from Haskell and C++ functions
```bash
stack test
```

load application in GHCI and run functions interactively
```bash
stack ghci  --ghci-options='-fobject-code -O0'
> HsLib.isTriple 3 4 5
> True
> InlineCPP.isTriple 3 4 5
> 1
```

load a single file in GHCI, for example InlineCPP
```bash
stack ghci src/InlineCPP.hs --ghci-options='-fobject-code -O0'
> isTriple 3 4 5
> 1
```

## Concept 
Extract a Haskell function (hsFunction) from a formal [Coq](https://coq.inria.fr/) proof that represents the specification of an existing C++ function (cppFunction).  The extracted Haskell function by definition will be verified as correct. This test bench represents an example of how to use [Haskell](https://www.haskell.org/), [inline-c-cpp](http://hackage.haskell.org/package/inline-c-cpp) and the property based testing tool [QuickCheck](http://hackage.haskell.org/package/QuickCheck) to verify that hsFunction and cppFunction produce identical results for the same inputs. 

## Example
### Haskell function
```Haskell
module HsLib ( square )

-- square a number
square :: Num a => a -> a
square x = x * x
```
### C++ function
```cpp
namespace cn {
    // square a number
    int square(int a) {
        return a * a;
    }
}
```
### Haskell function calling cn::square as inline-cpp
```Haskell
module InlineCPP ( square )

-- inline call to c++ 
square :: CInt -> IO CInt
square x = [C.exp| int { cn::square($(int x)) } |]
``` 
### Haskell QuickCheck test
```Haskell
module Main where

import           Control.Monad
import           Foreign.C.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified HsLib                         as HS
import qualified InlineCPP                     as CPP

-- quickCheck property calling the c++ and Haskell version
prop_square :: Property
prop_square = forAll arbitrary $ \n -> monadicIO $ do
    nn <- run $ CPP.square (n :: CInt)
    assert $ nn == HS.square n

-- run test 10,000 times with random integers
main :: IO ()
main =
    hspec
        $ describe "Haskell checking CPP"
        $ it "quickcheck: HS.square a == CPP.square a"
        $ quickCheck (withMaxSuccess 10000 prop_square)
```

### Run Test 
```bash
$ stack test
Haskell checking CPP
+++ OK, passed 10000 tests.
  quickcheck: HS.square a == CPP.square a
```