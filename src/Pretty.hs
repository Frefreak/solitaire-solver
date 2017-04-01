module Pretty where

import Types
import Data.List (transpose)

pretty :: Board -> String
pretty (Board (tl1, tl2, tl3) h (tr1, tr2, tr3) m) =
    let tl = map topleftConv [tl1, tl2, tl3]
        tr = map toprightConv [tr1, tr2, tr3]
        firstline = unwords tl ++ "   " ++ huaConv h ++ "  " ++ unwords tr
        pilestr = map (map prettyCard) m
        longest = maximum $ map length m
        pilestr' = map (pad longest) pilestr
        pad n ls = ls ++ replicate (n - length ls) "  "
    in unlines $ firstline : map unwords (transpose pilestr')

zhong :: String
zhong = "\ESC[31;1m\x1f004\ESC[0m"

fa :: String
fa = "\ESC[32;1m\x1f005\ESC[0m"

bai :: String
bai = "\ESC[30;1m\x1f006\ESC[0m"

hua :: String
hua = "\ESC[35;1m\x1F010\ESC[0m"

back :: String
back = "\ESC[37;1m\x1F02B\ESC[0m"

topleftConv :: TopLeftSlot -> String
topleftConv TLEmpty = "--"
topleftConv (TLSingleton n) = prettyCard n
topleftConv TLFull = back

toprightConv :: TopRightSlot -> String
toprightConv TREmpty = "--"
toprightConv (TRTaken n) = prettyCard n

huaConv :: HuaSlot -> String
huaConv HSEmpty = "--"
huaConv HSSingleton = hua

prettyCard :: Card -> String
prettyCard Zhong = zhong
prettyCard Fa = fa
prettyCard Bai = bai
prettyCard Hua = hua
prettyCard (Wan n) = "\ESC[30;1m" ++ show n ++ " \ESC[0m"
prettyCard (Tiao n) = "\ESC[32;1m" ++ show n ++ " \ESC[0m"
prettyCard (Tong n) = "\ESC[31;1m" ++ show n ++ " \ESC[0m"
