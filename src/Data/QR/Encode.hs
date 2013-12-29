module Data.QR.Encode where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Char
import Data.Word

import Data.QR.Types

countLength :: Version -> Mode -> Int
countLength v m = f c m
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
encodeData Byte xs = UTF8.encode xs >>= toBinary 8

encode :: Version -> Level -> Mode -> String -> [Word8]
encode v l m xs = toWords $ take total $ base ++ pad8 ++ cycle padding
  where
    base = mode m ++ count v m (length xs) ++ encodeData m xs ++ replicate 4 Z
    total = dataBits v l
    pad8 = replicate ((-(length base)) `mod` 8) Z
    padding = [O,O,O,Z,O,O,Z,Z,Z,Z,Z,O,Z,Z,Z,O]

dataBits :: Version -> Level -> Int
dataBits v l = values !! ((v - 1) * 4 + levelIndex l)
    where
      values =
        [ 152, 128, 104, 72
        , 272, 224, 176, 128
        , 440, 352, 272, 208
        , 640, 512, 384, 288
        , 864, 688, 496, 368
        , 1088, 864, 608, 480
        , 1248, 992, 704, 528
        , 1552, 1232, 880, 688
        , 1856, 1456, 1056, 800
        , 2192, 1728, 1232, 976
        , 2592, 2032, 1440, 1120
        , 2960, 2320, 1648, 1264
        , 3424, 2672, 1952, 1440
        , 3688, 2920, 2088, 1576
        , 4184, 3320, 2360, 1784
        , 4712, 3624, 2600, 2024
        , 5176, 4056, 2936, 2264
        , 5768, 4504, 3176, 2504
        , 6360, 5016, 3560, 2728
        , 6888, 5352, 3880, 3080
        , 7456, 5712, 4096, 3248
        , 8048, 6256, 4544, 3536
        , 8752, 6880, 4912, 3712
        , 9392, 7312, 5312, 4112
        , 10208, 8000, 5744, 4304
        , 10960, 8496, 6032, 4768
        , 11744, 9024, 6464, 5024
        , 12248, 9544, 6968, 5288
        , 13048, 10136, 7288, 5608
        , 13880, 10984, 7880, 5960
        , 14744, 11640, 8264, 6344
        , 15640, 12328, 8920, 6760
        , 16568, 13048, 9368, 7208
        , 17528, 13800, 9848, 7688
        , 18448, 14496, 10288, 7888
        , 19472, 15312, 10832, 8432
        , 20528, 15936, 11408, 8768
        , 21616, 16816, 12016, 9136
        , 22496, 17728, 12656, 9776
        , 23648, 18672, 13328, 10208 ]
