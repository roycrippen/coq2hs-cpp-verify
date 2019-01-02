module Main where

import qualified Data.Vector.Storable          as V
import qualified InlineCPP                     as CPP
import qualified HsLib                         as HS
import           Foreign.C
import           Control.Monad.IO.Class

main :: IO ()
main = do
    putStrLn "\nInline CPP compared to Haskell\n"

    -- example hs and cpp result compare
    cppSquare   <- CPP.square 3
    isCppTriple <- CPP.isTriple 3 4 5
    putStrLn $ "CPP.square 3: " ++ show cppSquare
    putStrLn $ " HS.square 3: " ++ show (HS.square 3)
    putStrLn $ "CPP.isTriple 3 4 5: " ++ show isCppTriple
    putStrLn $ " HS.isTriple 3 4 5: " ++ show (HS.isTriple 3 4 5)

    -- example sending a vector to cpp
    let xs = [10, 30, 60]
    x <- CPP.sumVec xs
    putStrLn $ "sum " ++ show xs ++ " = " ++ show x

    -- example getting a cpp created list 
    xs <- CPP.rangeList 10
    putStrLn $ "rangeList 10 = " ++ show xs ++ "\n"

    -- validate calling CPP.applyXorCipher with std::cout
    -- interesting way to quickly test the cpp code
    CPP.testApplyXorCipher

    -- CPP.applyXorCipher twice
    putStrLn ""
    let s1  = "String to cipher."
    let key = "my secret key"
    putStrLn $ "input string   = " ++ show s1
    s2 <- CPP.applyXorCipher s1 key
    putStrLn $ "encoded string from cpp = " ++ show s2
    s3 <- CPP.applyXorCipher s2 key
    putStrLn $ "decoded string from cpp = " ++ show s3
