module Data.QR.Encode where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Char
import Data.Maybe
import Data.Word

import Data.QR.Tables
import Data.QR.Types

countLength :: Version -> Mode -> Int
countLength v = f c
  where
    c | v < 10 = 0 :: Int
      | v < 27 = 1
      | otherwise = 2
    f 0 Numeric = 10
    f 1 Numeric = 12
    f _ Numeric = 14
    f 0 Alpha = 9
    f 1 Alpha = 11
    f _ Alpha = 13
    f 0 Byte = 8
    f _ Byte = 16

count :: Version -> Mode -> Int -> [Bit]
count v m = toBinary (countLength v m)

mode :: Mode -> [Bit]
mode = toBinary 4 . go
  where
    go Numeric = 1 :: Int
    go Alpha = 2
    go Byte = 4

encodeData :: Mode -> String -> [Bit]
encodeData Numeric xs = chunksOf 3 xs >>= encodeChunk
  where
    encodeChunk c = case reads c of
      [(n, "")] | n >= 0 -> toBinary (bits (length c)) (n :: Int)
      _ -> []
    bits 1 = 4
    bits 2 = 7
    bits _ = 10
encodeData Alpha xs = chunksOf 2 xs >>= encodeChunk
  where
    encodeChunk [x] = toBinary 6 (value x)
    encodeChunk [x, y] = toBinary 11 (value x * 45 + value y)
    encodeChunk _ = []

    value ' ' = 36
    value '$' = 37
    value '%' = 38
    value '*' = 39
    value '+' = 40
    value '-' = 41
    value '.' = 42
    value '/' = 43
    value ':' = 44
    value x
      | isAlpha x = ord (toUpper x) - ord 'A' + 10
      | isDigit x = digitToInt x
      | otherwise = 0
encodeData Byte xs = UTF8.encode xs >>= toBinary 8

encode :: Version -> Level -> Mode -> String -> [Word8]
encode v l m xs = toWords $ take total $ base ++ pad8 ++ cycle padding
  where
    base0 = mode m ++ count v m (length xs) ++ encodeData m xs
    base = take total $ base0 ++ replicate 4 Z
    total = dataBits v l
    pad8 = replicate ((-(length base)) `mod` 8) Z
    padding = [O,O,O,Z,O,O,Z,Z,Z,Z,Z,O,Z,Z,Z,O]

minimumVersion :: Level -> Mode -> Int -> Maybe Version
minimumVersion l m sz = fmap fst . listToMaybe . dropWhile ((< sz) . snd)
                      . zip [1..] . map (capacity l m)
                      $ [1 .. 40]
