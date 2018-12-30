# coq2hs-cpp-verify
Proof of concept test bench to code compare functions implemented in Haskell and C++.

requires haskell stack >= 1.9.8

build
```bash
stack build
```

run sample application calling both Haskell and C++ functions
```bash
stack run
```

run property based tests and unit tests comparing results from Haskell and C++ functions)
```bash
stack test
```
