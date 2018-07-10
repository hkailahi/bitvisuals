module Internal.BitOps
  ( Digits
  , toDigits
  , toDigitsHelper
  )
  where

type Digits = [Word]

toDigits :: Word -> Word -> Digits
toDigits _ 0    = [0]
toDigits base x = toDigitsHelper base x

toDigitsHelper :: Word -> Word -> Digits
toDigitsHelper _ 0    = []
toDigitsHelper base x =
  toDigitsHelper base (x `div` base) ++ [x `mod` base]
