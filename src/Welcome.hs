module Welcome where

import BitOps
import BitPrinters
import Control.Monad (join)
import Data.Foldable (foldl')
import qualified Data.List as L (intersperse)
import Data.Tuple.Curry (uncurryN)

import Text.Printf

printMethods :: [String]
printMethods = [ "- printBinAnd :: Int -> Int -> IO ()"
               , "- printBinOr :: Int -> Int -> IO ()"
               , "- printBinXOR :: Int -> Int -> IO ()"
               , "- printBinShiftR :: Int -> Int -> IO ()"
               , "- printBinShiftL :: Int -> Int -> IO ()"
               , "- printBinAdd :: Int -> Int -> IO ()"
               , "- printBinMinus :: Int -> Int -> IO ()"
               ]
bitOps :: [String]
bitOps = [ "** type Digits = [Int] **"
         , "- toBin :: Int -> String"
         , "- toDigits :: Int -> Digits"
         , "- toBinDigits :: Int -> Digits"
         , "- binLength :: Int -> Int"
         , "- digitsToString :: Digits -> String"
         , "- stringOfDigits :: Int -> String"
         , "- stringOfBinDigits :: Int -> String"
         , "- fromDigits :: Digits -> Int"
         , "- fromBinDigits :: Digits -> Int"
         , "- fromBinString :: String -> Int"
         , "- matchZeroes :: Int -> Int -> Digits"
         ]

bitAlgos :: [String]
bitAlgos = [ "- countSetBits :: Int -> Int"
           , "- hammingDistance :: Int -> Int -> Int"
           , "- reverseBits :: Int -> Int"
           , "- oddParity :: Int -> Bool"
           , "- rotateL12 :: Int -> Int -> Int"
           , "- rotateR12 :: Int -> Int -> Int"
           , "- rotateL12' :: Int -> Int -> Int"
           , "- rotateR12' :: Int -> Int -> Int"
           ]

printFns :: [String] -> String
printFns xs = foldl' (++) "" $ L.intersperse "\n" xs

main = do
  putStrLn
    $ uncurryN
        (printf "%s\n\n %s\n%s\n\n %s\n%s\n\n %s\n%s")
        ( "Welcome to the BitVisuals!"
        , " Try printing some basic bitwise operations:"
        , printFns printMethods
        , "Or compose your own function with these:"
        , printFns bitOps
        , "We've made useful algorithms for you too:"
        , printFns bitAlgos
        )
