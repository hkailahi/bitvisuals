module Main where

-- https://rosettacode.org/wiki/Binary_digits#Haskell
-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Bits.html
-- https://www.tutorialspoint.com/java/java_basic_operators.htm
-- https://www.tutorialspoint.com/java/java_bitwise_operators_examples.htm

import Data.List
import Text.Printf
import Data.Bits

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toBinDigits :: Int -> [Int]
toBinDigits x = toDigits (read (toBin x) :: Int)

stringDigit :: Int -> String
stringDigit i = foldl (++) "" $ fmap (show) (toDigits i)

stringBinDigit :: Int -> String
stringBinDigit i = foldl (++) "" $ fmap (show) (toBinDigits i)

digitsToString :: [Int] -> String
digitsToString d = foldl (++) "" $ fmap (show) d

binLength :: Int -> Int
binLength 0 = 0
binLength i = length $ toDigits (read (toBin i) :: Int)

matchZeroes :: Int -> Int -> [Int]
matchZeroes a b
  -- returns b with 0s prepended until equal or greater length than a
  | a == 0 && b == 0 = [0]
  | b >= a           = toBinDigits b
  | otherwise        = let zeroes = binLength a - binLength b
                       in replicate zeroes 0 ++ (if b == 0 then [] else toBinDigits b)

fromDigits :: [Int] -> Int
fromDigits []     = 0
fromDigits (x:[]) = x
fromDigits (x:xs) = (foldl (*) 1 $ x : (replicate (length xs) 10)) + fromDigits(xs)
-- fromDigits (x:xs) = ( last $ take (length xs + 1) $ iterate (*10) x) + fromDigits(xs)

fromBinDigits :: [Int] -> Int
fromBinDigits []     = 0
fromBinDigits (x:[]) = x
fromBinDigits (x:xs) = ( foldl (*) 1 $ x : (replicate (length xs) 2) ) + fromBinDigits(xs)
-- fromBinDigits (x:xs) = (last $ take (length xs + 1) $ iterate (*2) x) + fromBinDigits(xs)

toBin :: (Integral a, Show a) => a -> [Char]
toBin 0 = []
toBin x =  (toBin $ x `div` 2) ++ (show $ x `mod` 2)

-- printToBin :: (PrintfArg a, Integral a, Show a) => a -> IO ()
-- printToBin n = putStrLn $ printf "%4d  %14s" n (toBin n)

-- two's complement negative has 1 at head
-- bComplement :: Int -> Int
-- bComplement i = i * (negate 1) - 1
--
-- toNegBin :: (Integral a, Show a) => a -> [Char]
-- toNegBin 0 = []
-- toNegBin x = (toBin $ (negate x) `div` 2) ++ (show $ (negate x) `mod` 2)
--
-- toTwosComp :: [Char] -> [Char]
-- toTwosComp c = fmap (\x -> if (x == '0') then '1' else '0') c
--
-- negativeToDigits :: Int -> [Int]
-- negativeToDigits x = toDigits (read (toNegBin x) :: Int)

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

-- ************************** INDIVIAUAL OPERATION PRINTING METHODS ************************** --

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

-- -- ~ (bitwise compliment)
-- printBinComplement :: Int -> IO ()
-- printBinComplement a = let result = complement a
--                        in putStrLn $ printf "~\t%-d %+14s\n------------------------------\n\t%-d %+14s" (a) (toBin a) (result) (toBin result)

-- << (left shift)
printBinShiftL :: Int -> Int -> IO ()
printBinShiftL a b = let result = shiftL a b -- shift a left b times
                     in putStrLn $ printf "Decimal:\t%-d << %-d = %-d\n------------------------------\nBinary:%+8s << %-d = %+8s" (a) (b) (result) (toBin a) (b) (toBin result)

-- >> (right shift)
printBinShiftR :: Int -> Int -> IO ()
printBinShiftR a b = let result = shiftR a b -- shift a right b times
                     in putStrLn $ printf "Decimal:\t%-d >> %-d = %-d\n------------------------------\nBinary:%+8s >> %-d = %+8s" (a) (b) (result) (toBin a) (b) (toBin result)

-- >>> (zero fill right shift)
-- printBinZFSR :: Int -> Int -> IO ()
-- printBinZFSR a b = let result = xor a b
--                    in putStrLn $ printf "\t%-d %+14s\n>>>\t%-d %+14s\n------------------------------\n\t%-d %+14s" (a) (toBin a) (b) (toBin b) (result) (toBin result)

printMethodsInBitVisual :: String
printMethodsInBitVisual =  "printBinAnd :: Int -> Int -> IO ()" ++ "\n"
                        ++ "printBinOr :: Int -> Int -> IO ()" ++ "\n"
                        ++ "printBinXOR :: Int -> Int -> IO ()" ++ "\n"
                        ++ "printBinShiftR :: Int -> Int -> IO ()" ++ "\n"
                        ++ "printBinShiftL :: Int -> Int -> IO ()" ++ "\n"
                        ++ "printBinAdd :: Int -> Int -> IO ()" ++ "\n"
                        ++ "printBinMinus :: Int -> Int -> IO ()" ++ "\n"

methodsInBitVisual :: String
methodsInBitVisual =  "toDigits :: Integral x => x -> [x]" ++ "\n"
                   ++ "toBinDigits :: Int -> [Int]" ++ "\n"
                   ++ "stringDigit :: Int -> String" ++ "\n"
                   ++ "stringBinDigit :: Int -> String" ++ "\n"
                   ++ "digitsToString :: [Int] -> String" ++ "\n"
                   ++ "binLength :: Int -> Int" ++ "\n"
                   ++ "matchZeroes :: Int -> Int -> [Int]" ++ "\n"
                   ++ "fromDigits :: [Int] -> Int" ++ "\n"
                   ++ "fromBinDigits :: [Int] -> Int" ++ "\n"
                   ++ "toBin :: (Integral a, Show a) => a -> [Char]" ++ "\n"

main = do
  putStrLn $ printf "%s\n\n %s\n%s\n\n %s\n%s" "Welcome to the BitVisual!" " Try a bitwise operations:" printMethodsInBitVisual "Or compose your own function with these:" methodsInBitVisual
