module BitAlgos where

import BitOps

import Data.Bits

countSetBits :: Int -> Int -- Kernighan's Algorithm
countSetBits 0 = 0
countSetBits n = let x = n .&. (n-1)
                 in 1 + countSetBits(x)

hammingDistance :: Int -> Int -> Int
hammingDistance a b
  | binLength a == binLength b = countSetBits (xor a b)
  | otherwise                  = error "Lengths of both strings must be equal."
  -- ^ Though the first case works for different bit lengths,
  -- the Hamming Distance definition requires the comparison
  -- to be between equal length strings (matching leading 1's)

-- NOTE: All rotation methods are hardcoded for 12 bit rotation

rotateL12 :: Int -> Int -> Int
rotateL12 n d = let rotations = mod d 12
                    bitsize = 12
                    bitMask = fromBinDigits $ take bitsize [1,1..]
                    -- xxx00000 get first (12 - rotations) bits
                    front   = shiftL (shiftR bitMask (rotations) .&. n) rotations
                    -- 000xxxxx get rest (rotations) bits
                    back    = shiftR n (bitsize - rotations)
                in front .|. back

rotateR12 :: Int -> Int -> Int
rotateR12 n d = let rotations = mod d 12
                    bitsize = 12
                    bitMask = fromBinDigits $ take bitsize [1,1..]
                    -- xxx00000 get first rotations bits
                    front   = shiftL ((shiftR bitMask (bitsize - rotations)) .&. n) (bitsize - rotations)
                    -- 000xxxxx get rest (12 - rotations) bits
                    back    = (shiftR n rotations)
                in front .|. back