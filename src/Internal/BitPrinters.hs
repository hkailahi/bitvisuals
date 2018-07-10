module Internal.BitPrinters
    ( blend
    , printBinOp'
    , printfFromParamList6
    ) where

import Text.Printf (printf)

import BitOps (digitsToString, matchZeroes)

printBinOp' :: Word -> Word -> Word -> [String]
printBinOp' a b result = let leadingZero x y = digitsToString $ matchZeroes x y
                             a' = leadingZero b a
                             b' = leadingZero a b
                             r'
                                | a > b     = leadingZero a result
                                | otherwise = leadingZero b result
                         in blend (fmap (show) [a,b,result]) [a', b', r']

blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x : (blend ys xs)
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
