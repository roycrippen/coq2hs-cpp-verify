module Main where

import qualified InlineCPP                     as CPP
import qualified HsLib                         as HS
import qualified Data.ByteString.Char8         as C
import           Text.Printf                              ( printf )
import           Data.Foldable                            ( for_ )

main :: IO ()
main = do
    putStrLn "\nInline CPP compared to Haskell\n"

    -- example hs and cpp result compare
    cppSquare   <- CPP.square 3
    isCppTriple <- CPP.isTriple 3 4 5
    putStrLn $ "CPP.square 3: " ++ show cppSquare
    putStrLn $ " HS.square 3: " ++ show (HS.square 3 :: Int)
    putStrLn $ "CPP.isTriple 3 4 5: " ++ show isCppTriple
    putStrLn $ " HS.isTriple 3 4 5: " ++ show (HS.isTriple 3 4 5)

    -- example sending a vector to cpp
    let xs = [10, 30, 60]
    x <- CPP.sumVec xs
    putStrLn $ "sum " ++ show xs ++ " = " ++ show x

    -- example getting a cpp created list 
    ys <- CPP.rangeList 10
    putStrLn $ "rangeList 10 = " ++ show ys ++ "\n"

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
    let s2' = HS.applyXorCipher s1 key
        s3' = HS.applyXorCipher s2 key
    putStrLn $ "input string   = " ++ show s1
    putStrLn $ "encoded string from hs = " ++ show s2'
    putStrLn $ "decoded string from hs = " ++ show s3'

    -- call  Haskell code-point -> utf8 and utf8 to code-point
    putStrLn "\nHaskell encoding"
    putStrLn "Character  Unicode  UTF-8 encoding (hex)  Decoded"
    putStrLn "-------------------------------------------------"
    let cps = [0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E]
    for_ cps $ \codepoint -> do
        let values     = HS.encodeCodepoint codepoint
            codepoint' = HS.decodeToCodepoint values
        putStrLn $ printf "%c          %-7s  %-20s  %c"
                          codepoint
                          (printf "U+%04X" codepoint :: String)
                          (unwords (map (printf "%02X") values))
                          codepoint'

    -- call  C++ code-point -> utf8 and utf8 to code-point
    putStrLn "\nC++ encoding"
    putStrLn "Character  Unicode  UTF-8 encoding (hex)  Decoded"
    putStrLn "-------------------------------------------------"
    for_ [0x0041, 0x00F6, 0x0416, 0x20AC, 0x1D11E] $ \codepoint -> do
        values     <- CPP.encodeCodepoint codepoint
        codepoint' <- CPP.decodeToCodepoint values
        putStrLn $ printf "%c          %-7s  %-20s  %c"
                          codepoint
                          (printf "U+%04X" codepoint :: String)
                          (unwords (map (printf "%02X") values))
                          codepoint'

