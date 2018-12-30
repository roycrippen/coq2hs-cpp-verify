module Main where

import qualified Data.Vector.Storable          as V
import qualified InlineCPP                     as CPP
import qualified HsLib                         as HS
import           Foreign.C

main :: IO ()
main = do
    cppSquare   <- CPP.square 3
    isCppTriple <- CPP.isTriple 3 4 5
    putStrLn "Inline CPP compared Haskell"
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
    putStrLn $ "foo xs = " ++ show xs

    -- validate calling CPP.encode and CPP.decode through std::cout
    CPP.testEncodeDecode

    -- full CPP.encode then CPP.decode test
    putStrLn ""
    let s1 = "This another test string..."
    putStrLn $ "input string   = " ++ s1
    zs <- CPP.encode s1
    putStrLn $ "encoded vector = " ++ show zs
    s2 <- CPP.decode zs
    putStrLn $ "decoded string = " ++ s2
