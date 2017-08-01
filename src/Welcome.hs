module Welcome where

import BitOps
import BitPrinters

import Text.Printf

allPrintMethods :: String
allPrintMethods =  "- printBinAnd :: Int -> Int -> IO ()" ++ "\n"
                ++ "- printBinOr :: Int -> Int -> IO ()" ++ "\n"
                ++ "- printBinXOR :: Int -> Int -> IO ()" ++ "\n"
                ++ "- printBinShiftR :: Int -> Int -> IO ()" ++ "\n"
                ++ "- printBinShiftL :: Int -> Int -> IO ()" ++ "\n"
                ++ "- printBinAdd :: Int -> Int -> IO ()" ++ "\n"
                ++ "- printBinMinus :: Int -> Int -> IO ()" ++ "\n"

allBitOps :: String
allBitOps =  "** type Digits = [Int] **" ++ "\n"
          ++ "- toBin :: Int -> String" ++ "\n"
          ++ "- toDigits :: Int -> Digits" ++ "\n"
          ++ "- toBinDigits :: Int -> Digits" ++ "\n"
          ++ "- binLength :: Int -> Int" ++ "\n"
          ++ "- digitsToString :: Digits -> String" ++ "\n"
          ++ "- stringOfDigits :: Int -> String" ++ "\n"
          ++ "- stringOfBinDigits :: Int -> String" ++ "\n"
          ++ "- fromDigits :: Digits -> Int" ++ "\n"
          ++ "- fromBinDigits :: Digits -> Int" ++ "\n"
          ++ "- fromBinString :: String -> Int" ++ "\n"
          ++ "- matchZeroes :: Int -> Int -> Digits" ++ "\n"

allBitAlgos :: String
allBitAlgos = "- countSetBits :: Int -> Int" ++ "\n"
            ++ "- hammingDistance :: Int -> Int -> Int" ++ "\n"
            ++ "- reverseBits :: Int -> Int" ++ "\n"
            ++ "- oddParity :: Int -> Bool" ++ "\n"
            ++ "- rotateL12 :: Int -> Int -> Int" ++ "\n"
            ++ "- rotateR12 :: Int -> Int -> Int" ++ "\n"
            ++ "- rotateL12' :: Int -> Int -> Int" ++ "\n"
            ++ "- rotateR12' :: Int -> Int -> Int" ++ "\n"

main = do
  putStrLn $ printf "%s\n\n %s\n%s\n\n %s\n%s\n\n %s\n%s" "Welcome to the BitVisuals!" " Try printing some basic bitwise operations:" allPrintMethods "Or compose your own function with these:" allBitOps "We've made useful algorithms for you too:" allBitAlgos
