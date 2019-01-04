module Main where

import qualified Data.Vector.Storable          as V
import qualified InlineCPP                     as CPP
import qualified HsLib                         as HS
import           Foreign.C
import           Control.Monad.IO.Class
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C

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

    -- call CPP.applyXorCipher twice
    putStrLn ""
    let s1  = C.pack "String to cipher."
        key = C.pack "my secret key"
    s2 <- CPP.applyXorCipher s1 key
    s3 <- CPP.applyXorCipher s2 key
    putStrLn $ "input string   = " ++ show s1
    putStrLn $ "encoded string from cpp = " ++ show s2
    putStrLn $ "decoded string from cpp = " ++ show s3

    -- call HS.applyXorCipher twice
    putStrLn ""
    let s1  = C.pack "String to cipher."
        key = C.pack "my secret key"
        s2  = HS.applyXorCipher s1 key
        s3  = HS.applyXorCipher s2 key
    putStrLn $ "input string   = " ++ show s1
    putStrLn $ "encoded string from hs = " ++ show s2
    putStrLn $ "decoded string from hs = " ++ show s3

