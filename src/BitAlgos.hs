{-# LANGUAGE ViewPatterns #-}

module BitAlgos where

import Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)

import BitOps ( binLength, fromBinDigits, fromBinString, toBinString)

countSetBits :: Word -> Word -- Kernighan's Algorithm
countSetBits 0 = 0
countSetBits n = 1 + countSetBits x
                   where
                     x = n .&. (n-1) -- weirdly inlining this requires a type annotation

hammingDistance :: Word -> Word -> Maybe Word
hammingDistance a b
  | binLength a == binLength b = Just . countSetBits $ xor a b
  | otherwise                  = Nothing -- Lengths of both strings must be equal.
  -- ^ Though the algorithm works for different bit lengths, the
  -- definition of the "Hamming Distance" algorithm requires the
  -- comparison to be between equal length bitstrings (matching leading 1's)

reverseBits :: Word -> Word
reverseBits n = fromBinString $ reverse $ toBinString n

oddParity :: Word -> Bool -- Checks if there is an odd number of 1-bits
oddParity n = ((mod (countSetBits n) 2) /= 0)

rotateL_12Bit :: Word -> Word -> Word
rotateL_12Bit n (fromIntegral -> d) =
  let rotations = mod d 12
      bitsize   = 12
      bitMask   = fromBinDigits $ take bitsize [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front     = shiftL (shiftR bitMask (rotations) .&. n) rotations
      -- 000xxxxx get rest (rotations) bits
      back      = shiftR n (bitsize - rotations)
  in front .|. back

rotateR_12Bit :: Word -> Word -> Word
rotateR_12Bit n (fromIntegral -> d) =
  let rotations = mod d 12
      bitsize   = 12
      bitMask   = fromBinDigits $ take bitsize [1,1..]
      -- xxx00000 get first rotations bits
      front     = shiftL ((shiftR bitMask (bitsize - rotations)) .&. n) (bitsize - rotations)
      -- 000xxxxx get rest (12 - rotations) bits
      back      = shiftR n rotations
  in front .|. back

rotateL_12Bit' :: Word -> Word -> Word
rotateL_12Bit' n (fromIntegral -> d) =
  let bitsize     = 12
      rotations   = mod d bitsize
      bMinusR     = bitsize - rotations
      shiftedMask = fromBinDigits $ take (bMinusR) [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front       = shiftL (shiftedMask .&. n) rotations
      -- 000xxxxx get rest (rotations) bits
      back        = shiftR n bMinusR
  in front .|. back

rotateR_12Bit' :: Word -> Word -> Word
rotateR_12Bit' n (fromIntegral -> d) =
  let bitsize       = 12
      rotations     = mod d bitsize
      bMinusR       = bitsize - rotations
      unshiftedMask = fromBinDigits $ take (rotations) [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front         = shiftL (unshiftedMask .&. n) bMinusR
      -- 000xxxxx get rest (rotations) bits
      back          = shiftR n (rotations)
  in front .|. back



rotateL_InPlace :: Word -> Word -> Word
rotateL_InPlace n (fromIntegral -> d) =
  let bitsize     = fromIntegral $ binLength n
      rotations   = mod d bitsize
      bMinusR     = bitsize - rotations
      shiftedMask = fromBinDigits $ take (bMinusR) [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front       = shiftL (shiftedMask .&. n) rotations
      -- 000xxxxx get rest (rotations) bits
      back        = shiftR n bMinusR
  in front .|. back

rotateR_InPlace :: Word -> Word -> Word
rotateR_InPlace n (fromIntegral -> d) =
  let bitsize       = fromIntegral $ binLength n
      rotations     = mod d bitsize
      bMinusR       = bitsize - rotations
      unshiftedMask = fromBinDigits $ take (rotations) [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front         = shiftL (unshiftedMask .&. n) bMinusR
      -- 000xxxxx get rest (rotations) bits
      back          = shiftR n (rotations)
  in front .|. back

rotateL_nBits :: Word -> Word -> Word -> Word
rotateL_nBits n (fromIntegral -> d) (fromIntegral -> bs) =
  let bitsize     = bs
      rotations   = mod d bitsize
      bMinusR     = bitsize - rotations
      shiftedMask = fromBinDigits $ take (bMinusR) [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front       = shiftL (shiftedMask .&. n) rotations
      -- 000xxxxx get rest (rotations) bits
      back        = shiftR n bMinusR
  in front .|. back

rotateR_nBits :: Word -> Word -> Word -> Word
rotateR_nBits n (fromIntegral -> d) (fromIntegral -> bs) =
  let bitsize       = bs
      rotations     = mod d bitsize
      bMinusR       = bitsize - rotations
      unshiftedMask = fromBinDigits $ take (rotations) [1,1..]
      -- xxx00000 get first (12 - rotations) bits
      front         = shiftL (unshiftedMask .&. n) bMinusR
      -- 000xxxxx get rest (rotations) bits
      back          = shiftR n (rotations)
  in front .|. back
