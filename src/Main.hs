module Main where

import BitOps
import BitPrinters

import Text.Printf

printMethodsInBitVisual :: String
printMethodsInBitVisual =  "- printBinAnd :: Int -> Int -> IO ()" ++ "\n"
                        ++ "- printBinOr :: Int -> Int -> IO ()" ++ "\n"
                        ++ "- printBinXOR :: Int -> Int -> IO ()" ++ "\n"
                        ++ "- printBinShiftR :: Int -> Int -> IO ()" ++ "\n"
                        ++ "- printBinShiftL :: Int -> Int -> IO ()" ++ "\n"
                        ++ "- printBinAdd :: Int -> Int -> IO ()" ++ "\n"
                        ++ "- printBinMinus :: Int -> Int -> IO ()" ++ "\n"

bitOpsInBitVisual :: String
bitOpsInBitVisual =   "** type Digits = [Int] **" ++ "\n"
                   ++ "- toBin :: Int -> String" ++ "\n"
                   ++ "- toDigits :: Int -> Digits" ++ "\n"
                   ++ "- toBinDigits :: Int -> Digits" ++ "\n"
                   ++ "- binLength :: Int -> Int" ++ "\n"
                   ++ "- digitsToString :: Digits -> String" ++ "\n"
                   ++ "- stringOfDigits :: Int -> String" ++ "\n"
                   ++ "- stringOfBinDigits :: Int -> String" ++ "\n"
                   ++ "- fromDigits :: Digits -> Int" ++ "\n"
                   ++ "- fromBinDigits :: Digits -> Int" ++ "\n"
                   ++ "- matchZeroes :: Int -> Int -> Digits" ++ "\n"
                   ++ "- countSetBits :: Int -> Int" ++ "\n"
                   ++ "- rotateR' :: Int -> Int -> Int" ++ "\n"
                   ++ "- rotateL' :: Int -> Int -> Int" ++ "\n"
                   ++ "- rotateL12 :: Int -> Int -> Int" ++ "\n"
                   ++ "- rotateR12 :: Int -> Int -> Int" ++ "\n"

main = do
  putStrLn $ printf "%s\n\n %s\n%s\n\n %s\n%s" "Welcome to the BitVisual!" " Try a bitwise operations:" printMethodsInBitVisual "Or compose your own function with these:" bitOpsInBitVisual
