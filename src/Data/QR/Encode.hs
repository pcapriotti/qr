module Data.QR.Encode where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Char
import Data.Maybe
import Data.Word

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

minimumVersion :: Level -> Mode -> Int -> Maybe Version
minimumVersion l m sz = listToMaybe . dropWhile (< sz) . map (capacity l m) $ [1 .. 40]

capacity :: Level -> Mode -> Version -> Int
capacity l m v = capacities !! (v * 16 + fromEnum l * 4 + fromEnum m)
  where
    capacities =
      [ 41, 25, 17, 10
      , 34, 20, 14, 8
      , 27, 16, 11, 7
      , 17, 10, 7, 4
      , 77, 47, 32, 20
      , 63, 38, 26, 16
      , 48, 29, 20, 12
      , 34, 20, 14, 8
      , 127, 77, 53, 32
      , 101, 61, 42, 26
      , 77, 47, 32, 20
      , 58, 35, 24, 15
      , 187, 114, 78, 48
      , 149, 90, 62, 38
      , 111, 67, 46, 28
      , 82, 50, 34, 21
      , 255, 154, 106, 65
      , 202, 122, 84, 52
      , 144, 87, 60, 37
      , 106, 64, 44, 27
      , 322, 195, 134, 82
      , 255, 154, 106, 65
      , 178, 108, 74, 45
      , 139, 84, 58, 36
      , 370, 224, 154, 95
      , 293, 178, 122, 75
      , 207, 125, 86, 53
      , 154, 93, 64, 39
      , 461, 279, 192, 118
      , 365, 221, 152, 93
      , 259, 157, 108, 66
      , 202, 122, 84, 52
      , 552, 335, 230, 141
      , 432, 262, 180, 111
      , 312, 189, 130, 80
      , 235, 143, 98, 60
      , 652, 395, 271, 167
      , 513, 311, 213, 131
      , 364, 221, 151, 93
      , 288, 174, 119, 74
      , 772, 468, 321, 198
      , 604, 366, 251, 155
      , 427, 259, 177, 109
      , 331, 200, 137, 85
      , 883, 535, 367, 226
      , 691, 419, 287, 177
      , 489, 296, 203, 125
      , 374, 227, 155, 96
      , 1022, 619, 425, 262
      , 796, 483, 331, 204
      , 580, 352, 241, 149
      , 427, 259, 177, 109
      , 1101, 667, 458, 282
      , 871, 528, 362, 223
      , 621, 376, 258, 159
      , 468, 283, 194, 120
      , 1250, 758, 520, 320
      , 991, 600, 412, 254
      , 703, 426, 292, 180
      , 530, 321, 220, 136
      , 1408, 854, 586, 361
      , 1082, 656, 450, 277
      , 775, 470, 322, 198
      , 602, 365, 250, 154
      , 1548, 938, 644, 397
      , 1212, 734, 504, 310
      , 876, 531, 364, 224
      , 674, 408, 280, 173
      , 1725, 1046, 718, 442
      , 1346, 816, 560, 345
      , 948, 574, 394, 243
      , 746, 452, 310, 191
      , 1903, 1153, 792, 488
      , 1500, 909, 624, 384
      , 1063, 644, 442, 272
      , 813, 493, 338, 208
      , 2061, 1249, 858, 528
      , 1600, 970, 666, 410
      , 1159, 702, 482, 297
      , 919, 557, 382, 235
      , 2232, 1352, 929, 572
      , 1708, 1035, 711, 438
      , 1224, 742, 509, 314
      , 969, 587, 403, 248
      , 2409, 1460, 1003, 618
      , 1872, 1134, 779, 480
      , 1358, 823, 565, 348
      , 1056, 640, 439, 270
      , 2620, 1588, 1091, 672
      , 2059, 1248, 857, 528
      , 1468, 890, 611, 376
      , 1108, 672, 461, 284
      , 2812, 1704, 1171, 721
      , 2188, 1326, 911, 561
      , 1588, 963, 661, 407
      , 1228, 744, 511, 315
      , 3057, 1853, 1273, 784
      , 2395, 1451, 997, 614
      , 1718, 1041, 715, 440
      , 1286, 779, 535, 330
      , 3283, 1990, 1367, 842
      , 2544, 1542, 1059, 652
      , 1804, 1094, 751, 462
      , 1425, 864, 593, 365
      , 3517, 2132, 1465, 902
      , 2701, 1637, 1125, 692
      , 1933, 1172, 805, 496
      , 1501, 910, 625, 385
      , 3669, 2223, 1528, 940
      , 2857, 1732, 1190, 732
      , 2085, 1263, 868, 534
      , 1581, 958, 658, 405
      , 3909, 2369, 1628, 1002
      , 3035, 1839, 1264, 778
      , 2181, 1322, 908, 559
      , 1677, 1016, 698, 430
      , 4158, 2520, 1732, 1066
      , 3289, 1994, 1370, 843
      , 2358, 1429, 982, 604
      , 1782, 1080, 742, 457
      , 4417, 2677, 1840, 1132
      , 3486, 2113, 1452, 894
      , 2473, 1499, 1030, 634
      , 1897, 1150, 790, 486
      , 4686, 2840, 1952, 1201
      , 3693, 2238, 1538, 947
      , 2670, 1618, 1112, 684
      , 2022, 1226, 842, 518
      , 4965, 3009, 2068, 1273
      , 3909, 2369, 1628, 1002
      , 2805, 1700, 1168, 719
      , 2157, 1307, 898, 553
      , 5253, 3183, 2188, 1347
      , 4134, 2506, 1722, 1060
      , 2949, 1787, 1228, 756
      , 2301, 1394, 958, 590
      , 5529, 3351, 2303, 1417
      , 4343, 2632, 1809, 1113
      , 3081, 1867, 1283, 790
      , 2361, 1431, 983, 605
      , 5836, 3537, 2431, 1496
      , 4588, 2780, 1911, 1176
      , 3244, 1966, 1351, 832
      , 2524, 1530, 1051, 647
      , 6153, 3729, 2563, 1577
      , 4775, 2894, 1989, 1224
      , 3417, 2071, 1423, 876
      , 2625, 1591, 1093, 673
      , 6479, 3927, 2699, 1661
      , 5039, 3054, 2099, 1292
      , 3599, 2181, 1499, 923
      , 2735, 1658, 1139, 701
      , 6743, 4087, 2809, 1729
      , 5313, 3220, 2213, 1362
      , 3791, 2298, 1579, 972
      , 2927, 1774, 1219, 750
      , 7089, 4296, 2953, 1817
      , 5596, 3391, 2331, 1435
      , 3993, 2420, 1663, 1024
      , 3057, 1852, 1273, 784 ]
