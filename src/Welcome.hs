module Welcome where

import Control.Monad (join)
import Data.Foldable (foldl')
import qualified Data.List as L (intersperse)
import Data.Tuple.Curry (uncurryN)

import Text.Printf

printMethods :: [String]
printMethods = [ "- printBinAnd    :: Word -> Word -> IO ()"
               , "- printBinOr     :: Word -> Word -> IO ()"
               , "- printBinXor    :: Word -> Word -> IO ()"
               , "- printBinShiftR :: Word -> Word -> IO ()"
               , "- printBinShiftL :: Word -> Word -> IO ()"
               , "- printBinAdd    :: Word -> Word -> IO ()"
               , "- printBinMinus  :: Word -> Word -> IO ()"
               ]

bitOps :: [String]
bitOps = [ "type Digits = [Word]"
         , "toBinString    :: Word -> String"
         , "toDecDigits    :: Word -> Digits"
         , "toBinDigits    :: Word -> Digits"
         , "binLength      :: Word -> Word"
         , "digitsToString :: Digits -> String"
         , "fromBinDigits  :: Digits -> Word"
         , "fromBinString  :: String -> Word"
         , "matchZeroes    :: Word -> Word -> Digits"
         ]

bitAlgos :: [String]
bitAlgos = [ "countSetBits :: Word -> Word"
           , "hammingDistance :: Word -> Word -> Maybe Word"
           , "reverseBits :: Word -> Word"
           , "oddParity :: Word -> Bool"
           , "rotateL_12Bit :: Word -> Word -> Word"
           , "rotateR_12Bit :: Word -> Word -> Word"
           , "rotateL_12Bit' :: Word -> Word -> Word"
           , "rotateR_12Bit' :: Word -> Word -> Word"
           , "rotateL_InPlace :: Word -> Word -> Word"
           , "rotateR_InPlace :: Word -> Word -> Word"
           , "rotateL_nBits :: Word -> Word -> Word -> Word"
           , "rotateR_nBits :: Word -> Word -> Word -> Word"
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
