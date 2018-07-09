module BitPrinters where

import BitOps

import Text.Printf
import Data.Bits

-- ************************** PRINTING HELPER METHODS ************************** --

printBinOp' :: Int -> Int -> Int -> [String]
printBinOp' a b result = let leadingZero x y = digitsToString $ matchZeroes x y
                             a' = leadingZero b a
                             b' = leadingZero a b
                             r'
                                | a > b     = leadingZero a result
                                | otherwise = leadingZero b result
                         in blend (fmap (show) [a,b,result]) [a', b', r']

blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

printfFromParamList6 :: String -> [String] -> IO ()
printfFromParamList6 op params = let nAndB = "\t%+5s %+14s\n"
                                     printable = "\tDecimal      Binary\n"
                                                 ++ nAndB
                                                 ++ op
                                                 ++ nAndB
                                                 ++ "------------------------------\n"
                                                 ++ nAndB
                                 in putStrLn $ printf printable ((!!) params 0)
                                                                ((!!) params 1)
                                                                ((!!) params 2)
                                                                ((!!) params 3)
                                                                ((!!) params 4)
                                                                ((!!) params 5)

-- ************************** BITWISE OPERATION PRINTING METHODS ************************** --

printBinAdd :: Int -> Int -> IO ()
printBinAdd a b = let result = (+) a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "+" params

printBinMinus :: Int -> Int -> IO ()
printBinMinus a b = let result -- a must be greater than b, else flip b and a
                          | a >= b    = (-) a b
                          | otherwise = (-) b a
                        params
                          | a >= b    = printBinOp' a b result
                          | otherwise = printBinOp' b a result
                     in printfFromParamList6 "-" params

-- & (bitwise and)
printBinAnd :: Int -> Int -> IO ()
printBinAnd a b = let result = (.&.) a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "&" params

-- | (bitwise or)
printBinOr :: Int -> Int -> IO ()
printBinOr a b = let result = (.|.) a b
                     params = printBinOp' a b result
                  in printfFromParamList6 "|" params

-- ^ (bitwise XOR)
printBinXOR :: Int -> Int -> IO ()
printBinXOR a b = let result = xor a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "^" params

-- << (left shift)
printBinShiftL :: Int -> Int -> IO ()
printBinShiftL a b = let result = shiftL a b -- shift a left b times
                     in putStrLn
                          $ printf
                              "Decimal:\t%-d << %-d = %-d\n------------------------------\nBinary:%+8s << %-d = %+8s"
                              a
                              b
                              result
                              (toBin a)
                              b
                              (toBin result)

-- >> (right shift)
printBinShiftR :: Int -> Int -> IO ()
printBinShiftR a b = let result = shiftR a b -- shift a right b times
                     in putStrLn
                          $ printf "Decimal:\t%-d >> %-d = %-d\n------------------------------\nBinary:%+8s >> %-d = %+8s" (a) (b) (result) (toBin a) (b) (toBin result)
