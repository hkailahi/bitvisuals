{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module BitOps
    ( binLength
    , Digits
    , digitsToString
    , fromBinDigits
    , fromBinString
    , matchZeroes
    , toBinString
    , toBinDigits
    , toDecDigits
    ) where

import Data.Foldable (foldl')
import Control.Monad (join)
import Data.Char (digitToInt, ord, chr)

import Internal.BitOps (Digits, toDigits, toDigitsHelper)

-- soon..
-- newtype BinString = BinString { runBS :: String} deriving (Eq, Show)
-- newtype BinDigits = BinDigits { runBD :: Digits} deriving (Eq, Show)
-- newtype DecDigits = DecDigits { runDD :: Digits} deriving (Eq, Show)

-- FIXME oh god it's all partial
-- no negative handling
-- This may be a job for... TYPE LEVEL NATURALS *)O o O(*
-- or I could just fix Digits and make it smart
-- or actually handle negs w/ 2's-complement stuff

toBinString :: Word -> String
toBinString 0 = []
toBinString x = toBinString (x `div` 2) ++ (show $ x `mod` 2)

toDecDigits :: Word -> Digits
toDecDigits = toDigits 10

toBinDigits :: Word -> Digits
toBinDigits = toDigits 2

binLength :: Word -> Word
binLength = fromIntegral . length . toBinString

digitsToString :: Digits -> String
digitsToString = join . (show <$>)

stringOfDigits :: Word -> String
stringOfDigits = digitsToString . toDecDigits

stringOfBinDigits :: Word -> String
stringOfBinDigits = digitsToString . toBinDigits

fromDecDigits :: Digits -> Word
fromDecDigits []     = 0
fromDecDigits (x:[]) = x
-- FIXME lololol
fromDecDigits (x:xs) = (foldl' (*) 1 $ x : (replicate (length xs) 10)) + fromDecDigits xs

fromBinDigits :: Digits -> Word
fromBinDigits []     = 0
fromBinDigits (x:[]) = x
fromBinDigits (x:xs) = (foldl' (*) 1 $ x : (replicate (length xs) 2) ) + fromBinDigits xs

-- ie "111" to 7, -- "101" to 5
fromBinString :: String -> Word
fromBinString []     = 0
fromBinString (x:[]) = fromIntegral $ ord x
fromBinString (x:xs) = (foldl' (*) 1 $ (fromIntegral $ ord x) : (replicate (length xs) 2) ) + fromBinString xs

matchZeroes :: Word -> Word -> Digits
matchZeroes a b
  -- returns b with 0s prepended until equal or greater length than a
  | a == 0 && b == 0 = [0]
  | b >= a           = toBinDigits b
  | otherwise        = replicate zeroes 0 ++ (if b == 0 then [] else toBinDigits b)
                         where
                           zeroes :: Int
                           zeroes = fromIntegral $ binLength a - binLength b
