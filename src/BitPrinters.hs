{-# LANGUAGE ViewPatterns #-}

module BitPrinters
    ( printBinAdd, printBinMinus
    , printBinAnd, printBinOr, printBinXor
    , printBinShiftL, printBinShiftR
    ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import Text.Printf (printf)

import BitOps (toBinString)
import Internal.BitPrinters

printBinAdd :: Word -> Word -> IO ()
printBinAdd a b = let result = (+) a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "+" params

printBinMinus :: Word -> Word -> IO ()
printBinMinus a b = let result -- a must be greater than b, else flip b and a
                          | a >= b    = (-) a b
                          | otherwise = (-) b a
                        params
                          | a >= b    = printBinOp' a b result
                          | otherwise = printBinOp' b a result
                     in printfFromParamList6 "-" params

-- & (bitwise and)
printBinAnd :: Word -> Word -> IO ()
printBinAnd a b = let result = (.&.) a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "&" params

-- | (bitwise or)
printBinOr :: Word -> Word -> IO ()
printBinOr a b = let result = (.|.) a b
                     params = printBinOp' a b result
                  in printfFromParamList6 "|" params

-- ^ (bitwise XOR)
printBinXor :: Word -> Word -> IO ()
printBinXor a b = let result = xor a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "^" params

-- << (left shift)
printBinShiftL :: Word -> Word -> IO ()
printBinShiftL a (fromIntegral -> b) =
  let result = shiftL a b -- shift a left b times
  in putStrLn
      $ printf
          "Decimal:\t%-d << %-d = %-d\n------------------------------\nBinary:%+8s << %-d = %+8s"
          a
          b
          result
          (toBinString a)
          b
          (toBinString result)

-- >> (right shift)
printBinShiftR :: Word -> Word -> IO ()
printBinShiftR a (fromIntegral -> b) =
   let result = shiftR a b -- shift a right b times
   in putStrLn
        $ printf "Decimal:\t%-d >> %-d = %-d\n------------------------------\nBinary:%+8s >> %-d = %+8s" (a) (b) (result) (toBinString a) (b) (toBinString result)
