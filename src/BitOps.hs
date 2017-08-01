module BitOps where

import Data.List
import Text.Printf
import Data.Bits
import Control.Monad
import Control.Applicative

type Digits = [Int]

toBin :: Int -> String
toBin 0 = []
toBin x =  (toBin $ x `div` 2) ++ (show $ x `mod` 2)

toDigits :: Int -> Digits
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toBinDigits :: Int -> Digits
toBinDigits x = toDigits (read (toBin x) :: Int)

binLength :: Int -> Int
binLength 0 = 0
binLength i = length $ toDigits (read (toBin i) :: Int)

digitsToString :: Digits -> String
digitsToString d = foldl (++) "" $ fmap (show) d

stringOfDigits :: Int -> String
stringOfDigits i = digitsToString (toDigits i)

stringOfBinDigits :: Int -> String
stringOfBinDigits i = digitsToString (toBinDigits i)

fromDigits :: Digits -> Int
fromDigits []     = 0
fromDigits (x:[]) = x
fromDigits (x:xs) = (foldl (*) 1 $ x : (replicate (length xs) 10)) + fromDigits(xs)
-- fromDigits (x:xs) = ( last $ take (length xs + 1) $ iterate (*10) x) + fromDigits(xs)

fromBinDigits :: Digits -> Int
fromBinDigits []     = 0
fromBinDigits (x:[]) = x
fromBinDigits (x:xs) = ( foldl (*) 1 $ x : (replicate (length xs) 2) ) + fromBinDigits(xs)
-- fromBinDigits (x:xs) = (last $ take (length xs + 1) $ iterate (*2) x) + fromBinDigits(xs)

matchZeroes :: Int -> Int -> Digits
matchZeroes a b
  -- returns b with 0s prepended until equal or greater length than a
  | a == 0 && b == 0 = [0]
  | b >= a           = toBinDigits b
  | otherwise        = let zeroes = binLength a - binLength b
                       in replicate zeroes 0 ++ (if b == 0 then [] else toBinDigits b)
