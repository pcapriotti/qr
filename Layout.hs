module Layout where

import Control.Applicative
import Data.Array

type Coord = (Int, Int)
type Version = Int
data Module = Unknown | Light | Dark
  deriving (Eq, Ord, Read, Show, Enum)
type Matrix = Array Coord Module

size :: Version -> Int
size v = v * 4 + 17

alignment :: Version -> [Int]
alignment 1 = []
alignment 2 = [6, 18]
alignment 3 = [6, 22]
alignment 4 = [6, 26]
alignment 5 = [6, 30]
alignment 6 = [6, 34]
alignment 7 = [6, 22, 38]
alignment 8 = [6, 24, 42]
alignment 9 = [6, 26, 46]
alignment 10 = [6, 28, 50]
alignment 11 = [6, 30, 54]
alignment 12 = [6, 32, 58]
alignment 13 = [6, 34, 62]
alignment 14 = [6, 26, 46, 66]
alignment 15 = [6, 26, 48, 70]
alignment 16 = [6, 26, 50, 74]
alignment 17 = [6, 30, 54, 78]
alignment 18 = [6, 30, 56, 82]
alignment 19 = [6, 30, 58, 86]
alignment 20 = [6, 34, 62, 90]
alignment 21 = [6, 28, 50, 72, 94]
alignment 22 = [6, 26, 50, 74, 98]
alignment 23 = [6, 30, 54, 78, 102]
alignment 24 = [6, 28, 54, 80, 106]
alignment 25 = [6, 32, 58, 84, 110]
alignment 26 = [6, 30, 58, 86, 114]
alignment 27 = [6, 34, 62, 90, 118]
alignment 28 = [6, 26, 50, 74, 98, 122]
alignment 29 = [6, 30, 54, 78, 102, 126]
alignment 30 = [6, 26, 52, 78, 104, 130]
alignment 31 = [6, 30, 56, 82, 108, 134]
alignment 32 = [6, 34, 60, 86, 112, 138]
alignment 33 = [6, 30, 58, 86, 114, 142]
alignment 34 = [6, 34, 62, 90, 118, 146]
alignment 35 = [6, 30, 54, 78, 102, 126, 150]
alignment 36 = [6, 24, 50, 76, 102, 128, 154]
alignment 37 = [6, 28, 54, 80, 106, 132, 158]
alignment 38 = [6, 32, 58, 84, 110, 136, 162]
alignment 39 = [6, 26, 54, 82, 110, 138, 166]
alignment 40 = [6, 30, 58, 86, 114, 142, 170]

reserved :: Version -> Coord -> Bool
reserved = undefined

neighbours :: Int -> Coord -> [Coord]
neighbours k (x, y) = concat
  [ (,) <$> [x-k,x+k] <*> [y-k..y+k]
  , (,) <$> [x-k+1..x+k-1] <*> [y-k,y+k] ]

alignmentPatterns :: Version -> [(Coord, Module)]
alignmentPatterns v = centers >>= pattern
  where
    cs = alignment v
    sz = size v
    centers = filter valid ((,) <$> cs <*> cs)
    valid (x, y) = not $ or
      [ x < 10 && y < 10
      , x < 10 && y > sz - 11
      , x > sz - 11 && y < 10 ]
    pattern c = [(x, Dark) | x <- c : neighbours 2 c]
             ++ [(x, Light) | x <- neighbours 1 c]

finderPatterns :: Version -> [(Coord, Module)]
finderPatterns v = centers >>= pattern
  where
    sz = size v
    centers = [(3, 3), (sz - 4, 3), (3, sz - 4)]
    pattern c = [(x, Dark) | x <- c : neighbours 1 c ++ neighbours 3 c ]
              ++ [(x, Light) | x <- neighbours 2 c ++ neighbours 4 c
                             , valid x ]
    valid (x, y) = x >= 0 && x < sz && y >= 0 && y < sz

timingPatterns :: Version -> [(Coord, Module)]
timingPatterns v = pattern ++ map swap pattern
  where
    sz = size v
    pattern = [((x, 6), col x) | x <- [8 .. sz - 9]]
    col x = if x `mod` 2 == 0 then Dark else Light
    swap ((x, y), t) = ((y, x), t)

mkMatrix :: Version -> [(Coord, Module)] -> Matrix
mkMatrix v ms = accumArray max Unknown ((0, 0), (sz-1, sz-1)) ms
  where
    sz = size v

showModule :: Module -> Char
showModule Light = '-'
showModule Dark = '*'
showModule Unknown = ' '

showMatrix :: Matrix -> String
showMatrix m = unlines $ map row [0 .. h]
  where
    (_, (w, h)) = bounds m
    row y = map (\x -> tile x y) [0 .. w]
    tile x y = showModule $ m ! (x, y)
