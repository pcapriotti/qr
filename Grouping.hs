module Grouping where

import Data.List
import Data.Word

import Types
import ReedSolomon
import Encode

data Grouping = Grouping
  { group1 :: Group
  , group2 :: Group
  , ecSize :: Int }
  deriving (Eq, Ord, Show, Read)

data Group = Group
  { gpBlocks :: Int
  , gpSize :: Int }
  deriving (Eq, Ord, Show, Read)

groupSize :: Group -> Int
groupSize (Group s b) = s * b

interleave :: [[a]] -> [a]
interleave = concat . transpose

message :: Version -> Level -> Mode -> String -> [Word8]
message v l m xs = fullMessage v l (encode v l m xs)

fullMessage :: Version -> Level -> [Word8] -> [Word8]
fullMessage v l ws = interleave blocks ++ interleave ec
  where
    gpng = grouping v l
    (part1, part2) = splitAt (groupSize (group1 gpng)) ws
    blocks = chunksOf (gpSize (group1 gpng)) part1
          ++ chunksOf (gpSize (group2 gpng)) part2
    ec = map (errorWords (ecSize gpng)) blocks

grouping :: Version -> Level -> Grouping
grouping v l = gps !! ((v - 1) * 4 + levelIndex l)
  where
    gp :: Int -> Int -> Int -> Int -> Int -> Grouping
    gp ec g1 b1 g2 b2 = Grouping (Group g1 b1) (Group g2 b2) ec

    gps =
      [ gp 7 1 19 0 0
      , gp 10 1 16 0 0
      , gp 13 1 13 0 0
      , gp 17 1 9 0 0
      , gp 10 1 34 0 0
      , gp 16 1 28 0 0
      , gp 22 1 22 0 0
      , gp 28 1 16 0 0
      , gp 15 1 55 0 0
      , gp 26 1 44 0 0
      , gp 18 2 17 0 0
      , gp 22 2 13 0 0
      , gp 20 1 80 0 0
      , gp 18 2 32 0 0
      , gp 26 2 24 0 0
      , gp 16 4 9 0 0
      , gp 26 1 108 0 0
      , gp 24 2 43 0 0
      , gp 18 2 15 2 16
      , gp 22 2 11 2 12
      , gp 18 2 68 0 0
      , gp 16 4 27 0 0
      , gp 24 4 19 0 0
      , gp 28 4 15 0 0
      , gp 20 2 78 0 0
      , gp 18 4 31 0 0
      , gp 18 2 14 4 15
      , gp 26 4 13 1 14
      , gp 24 2 97 0 0
      , gp 22 2 38 2 39
      , gp 22 4 18 2 19
      , gp 26 4 14 2 15
      , gp 30 2 116 0 0
      , gp 22 3 36 2 37
      , gp 20 4 16 4 17
      , gp 24 4 12 4 13
      , gp 18 2 68 2 69
      , gp 26 4 43 1 44
      , gp 24 6 19 2 20
      , gp 28 6 15 2 16
      , gp 20 4 81 0 0
      , gp 30 1 50 4 51
      , gp 28 4 22 4 23
      , gp 24 3 12 8 13
      , gp 24 2 92 2 93
      , gp 22 6 36 2 37
      , gp 26 4 20 6 21
      , gp 28 7 14 4 15
      , gp 26 4 107 0 0
      , gp 22 8 37 1 38
      , gp 24 8 20 4 21
      , gp 22 12 11 4 12
      , gp 30 3 115 1 116
      , gp 24 4 40 5 41
      , gp 20 11 16 5 17
      , gp 24 11 12 5 13
      , gp 22 5 87 1 88
      , gp 24 5 41 5 42
      , gp 30 5 24 7 25
      , gp 24 11 12 7 13
      , gp 24 5 98 1 99
      , gp 28 7 45 3 46
      , gp 24 15 19 2 20
      , gp 30 3 15 13 16
      , gp 28 1 107 5 108
      , gp 28 10 46 1 47
      , gp 28 1 22 15 23
      , gp 28 2 14 17 15
      , gp 30 5 120 1 121
      , gp 26 9 43 4 44
      , gp 28 17 22 1 23
      , gp 28 2 14 19 15
      , gp 28 3 113 4 114
      , gp 26 3 44 11 45
      , gp 26 17 21 4 22
      , gp 26 9 13 16 14
      , gp 28 3 107 5 108
      , gp 26 3 41 13 42
      , gp 30 15 24 5 25
      , gp 28 15 15 10 16
      , gp 28 4 116 4 117
      , gp 26 17 42 0 0
      , gp 28 17 22 6 23
      , gp 30 19 16 6 17
      , gp 28 2 111 7 112
      , gp 28 17 46 0 0
      , gp 30 7 24 16 25
      , gp 24 34 13 0 0
      , gp 30 4 121 5 122
      , gp 28 4 47 14 48
      , gp 30 11 24 14 25
      , gp 30 16 15 14 16
      , gp 30 6 117 4 118
      , gp 28 6 45 14 46
      , gp 30 11 24 16 25
      , gp 30 30 16 2 17
      , gp 26 8 106 4 107
      , gp 28 8 47 13 48
      , gp 30 7 24 22 25
      , gp 30 22 15 13 16
      , gp 28 10 114 2 115
      , gp 28 19 46 4 47
      , gp 28 28 22 6 23
      , gp 30 33 16 4 17
      , gp 30 8 122 4 123
      , gp 28 22 45 3 46
      , gp 30 8 23 26 24
      , gp 30 12 15 28 16
      , gp 30 3 117 10 118
      , gp 28 3 45 23 46
      , gp 30 4 24 31 25
      , gp 30 11 15 31 16
      , gp 30 7 116 7 117
      , gp 28 21 45 7 46
      , gp 30 1 23 37 24
      , gp 30 19 15 26 16
      , gp 30 5 115 10 116
      , gp 28 19 47 10 48
      , gp 30 15 24 25 25
      , gp 30 23 15 25 16
      , gp 30 13 115 3 116
      , gp 28 2 46 29 47
      , gp 30 42 24 1 25
      , gp 30 23 15 28 16
      , gp 30 17 115 0 0
      , gp 28 10 46 23 47
      , gp 30 10 24 35 25
      , gp 30 19 15 35 16
      , gp 30 17 115 1 116
      , gp 28 14 46 21 47
      , gp 30 29 24 19 25
      , gp 30 11 15 46 16
      , gp 30 13 115 6 116
      , gp 28 14 46 23 47
      , gp 30 44 24 7 25
      , gp 30 59 16 1 17
      , gp 30 12 121 7 122
      , gp 28 12 47 26 48
      , gp 30 39 24 14 25
      , gp 30 22 15 41 16
      , gp 30 6 121 14 122
      , gp 28 6 47 34 48
      , gp 30 46 24 10 25
      , gp 30 2 15 64 16
      , gp 30 17 122 4 123
      , gp 28 29 46 14 47
      , gp 30 49 24 10 25
      , gp 30 24 15 46 16
      , gp 30 4 122 18 123
      , gp 28 13 46 32 47
      , gp 30 48 24 14 25
      , gp 30 42 15 32 16
      , gp 30 20 117 4 118
      , gp 28 40 47 7 48
      , gp 30 43 24 22 25
      , gp 30 10 15 67 16
      , gp 30 19 118 6 119
      , gp 28 18 47 31 48
      , gp 30 34 24 34 25
      , gp 30 20 15 61 16 ]
