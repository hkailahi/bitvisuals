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

countSetBits :: Int -> Int -- Kernighan's Algorithm
countSetBits 0 = 0
countSetBits n = let x = n .&. (n-1)
                 in 1 + countSetBits(x)

hammingDistance :: Int -> Int -> Int
hammingDistance a b
  | binLength a == binLength b = countSetBits (xor a b)
  | otherwise                  = error "Lengths of both strings must be equal."
  -- ^ Though the first case works for different bit lengths,
  -- I believe the Hamming Distance definition requires
  -- the comparison among equal strings

-- NOTE: All rotation methods are hardcoded for 12 bit rotation
-- Easy to

-- 12 bit rotation
rotateR' :: Int -> Int -> Int
rotateR' n d = let bitsize = 12
               in ((shiftR n d) .&. 4095) .|. (shiftL n (bitsize - d))

rotateL' :: Int -> Int -> Int
rotateL' n d = let bitsize = 12
               in ((shiftL n d) .&. 4095) .|. (shiftR n (bitsize - d))

rotateL12 :: Int -> Int -> Int
rotateL12 n d = let bitsize = 12
                    bitMask = fromBinDigits $ take bitsize [1,1..]
                    -- xxx00000 get first (12 - d) bits
                    front   = shiftL (shiftR bitMask (d) .&. n) d
                    -- 000xxxxx get rest (d) bits
                    back    = shiftR n (bitsize - d)
                in front .|. back

rotateR12 :: Int -> Int -> Int
rotateR12 n d = let bitsize = 12
                    bitMask = fromBinDigits $ take bitsize [1,1..]
                    -- xxx00000 get first d bits
                    front   = shiftL ((shiftR bitMask (bitsize - d)) .&. n) (bitsize - d)
                    -- 000xxxxx get rest (12 - d) bits
                    back    = (shiftR n d)
                in front .|. back
