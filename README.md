# coq2hs-cpp-verify
Proof of concept test bench to code compare functions implemented in Haskell and C++.

requires haskell stack >= 1.9.8

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

load Main in GHCI and test functions
```bash
stack ghci  --ghci-options='-fobject-code -O0'
> HsLib.isTriple 3 4 5
> True
> InlineCPP.isTriple 3 4 5
> 1
```

load InlineCPP in GHCI
```bash
stack ghci src/InlineCPP.hs --ghci-options='-fobject-code -O0'
> isTriple 3 4 5
> 1
```

