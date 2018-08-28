module Data.QR.Layout where

import Data.Array
import Data.Bits
import Data.Function
import Data.List
import Data.Word

import Data.QR.Types

type Coord = (Int, Int)
data Module = Empty | Reserved | Light | Dark
  deriving (Eq, Ord, Read, Show, Enum)
type Matrix = Array Coord Module

size :: Version -> Int
size v = v * 4 + 17

bitToModule :: Bit -> Module
bitToModule Z = Light
bitToModule O = Dark

maskModule :: Int -> Coord -> Bool
maskModule 0 (x, y) = (x + y) `mod` 2 == 0
maskModule 1 (_, y) = y `mod` 2 == 0
maskModule 2 (x, _) = x `mod` 3 == 0
maskModule 3 (x, y) = (x + y) `mod` 3 == 0
maskModule 4 (x, y) = (x `div` 3 + y `div` 2) `mod` 2 == 0
maskModule 5 (x, y) = let z = x * y in z `mod` 2 + z `mod` 3 == 0
maskModule 6 (x, y) = let z = x * y in (z `mod` 2 + z `mod` 3) `mod` 2 == 0
maskModule 7 (x, y) = ((x + y) `mod` 2 + (x * y) `mod` 3) `mod` 2 == 0
maskModule n _ = error $ "maskModule: invalid mask " ++ show n

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

darkModule :: Version -> [(Coord, Module)]
darkModule v = [((8, 4 * v + 9), Dark)]

reservedAreas :: Version -> [Coord]
reservedAreas v = (8,8)
    : [(8, y) | y <- [0 .. 5] ++ [7] ++ [sz - 7 .. sz - 1] ]
   ++ [(x, 8) | x <- [0 .. 5] ++ [7] ++ [sz - 8 .. sz - 1] ]
   ++ [c | v >= 7
         , x <- [sz - 11 .. sz - 9]
         , y <- [0 .. 5]
         , c <- sym x y ]
  where
    sz = size v
    sym x y = [(x, y), (y, x)]

reservedPatterns :: Version -> [(Coord, Module)]
reservedPatterns v = [(c, Reserved) | c <- reservedAreas v]

showModule :: Module -> Char
showModule Light = '-'
showModule Dark = '*'
showModule Empty = ' '
showModule Reserved = 'x'

showMatrix :: Matrix -> String
showMatrix m = unlines $ map row [0 .. h]
  where
    (_, (w, h)) = bounds m
    row y = map (\x -> tile x y) [0 .. w]
    tile x y = showModule $ m ! (x, y)

placement :: Matrix -> [Coord]
placement m = filter available cs
  where
    available c = m ! c == Empty
    (_, (_, n)) = bounds m
    line (x, r) =
      [ c | y <- if r then [n, n-1 .. 0] else [0 .. n]
          , c <- [(x, y), (x-1, y)] ]
    cols = zip ([n, n-2 .. 8] ++ [5, 3, 1])
               (cycle [True, False])
    cs = cols >>= line

placeBits :: Matrix -> [Word8] -> [(Coord, Module)]
placeBits m = placeWords (placement m)
  where
    bits :: Word8 -> [Module]
    bits w = map (\mask -> if w .&. mask == 0 then Light else Dark)
                 (take 8 (iterate (`shiftR` 1) 0x80))

    placeWord :: [Coord] -> Word8 -> ([Coord], [(Coord, Module)])
    placeWord cs w = case splitAt 8 cs of
      (cs1, cs') -> (cs', zip cs1 (bits w))

    placeWords :: [Coord] -> [Word8] -> [(Coord, Module)]
    placeWords [] _ = []
    placeWords cs [] = zip cs (repeat Light)
    placeWords cs (w : ws') = case placeWord cs w of
      (cs', ms) -> ms ++ placeWords cs' ws'

mkMatrix :: Version -> [(Coord, Module)] -> Matrix
mkMatrix v = accumArray max Empty ((0, 0), (sz-1, sz-1))
  where
    sz = size v

baseMatrix :: Version -> Matrix
baseMatrix v = mkMatrix v $ concat
  [ finderPatterns v
  , alignmentPatterns v
  , timingPatterns v
  , darkModule v
  , reservedPatterns v ]

maskedMatrices :: Version -> [Word8] -> [Matrix]
maskedMatrices v ws = map (\i -> mat // allBits i) [0..7]
  where
    mat = baseMatrix v
    ms = placeBits mat ws
    maskedBits i = map (\(c, m) -> (c, if maskModule i c then invert m else m)) ms
    reservedBits = [(c, Light) | c <- reservedAreas v]
    allBits i = maskedBits i ++ reservedBits

    invert Light = Dark
    invert Dark = Light
    invert m = m

formatBits :: Level -> Int -> Matrix -> [(Coord, Module)]
formatBits l k m = zip cs1 info ++ zip cs2 info
  where
    (_, (n,_)) = bounds m
    cs1 = [(x, 8) | x <- [0,1,2,3,4,5,7,8]]
       ++ [(8, y) | y <- [7,5,4,3,2,1,0]]
    cs2 = [(8, y) | y <- [n, n-1 .. n-7]]
       ++ [(x, 8) | x <- [n-6, n-5 .. n]]

    info = map bitToModule $ formatInfo l k

versionBits :: Version -> Matrix -> [(Coord, Module)]
versionBits v m | v > 6 = zip cs1 info ++ zip cs2 info
                | otherwise = []
  where
    (_, (n,_)) = bounds m
    cs1 = [(x, y) | x <- [5,4,3,2,1,0]
                  , y <- [n-8,n-9,n-10]]
    cs2 = [(x, y) | y <- [5,4,3,2,1,0]
                  , x <- [n-8,n-9,n-10]]
    info = map bitToModule $ versionInfo v

layout :: Version -> Level -> [Word8] -> Matrix
layout v l ws = mat // (formatBits l k mat ++ versionBits v mat)
  where
    mats = zip [0..] (maskedMatrices v ws)
    (k, mat) = minimumBy (compare `on` matScore) mats
    matScore (_, m) = score m

rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x : xs) = case span (== x) xs of
  (xs1, xs') -> (x, length xs1 + 1) : rle xs'

score1 :: Matrix -> Int
score1 m = sum (map s rows) + sum (map s cols)
  where
    (_, (n, _)) = bounds m
    s = sum . map s0 . rle
    s0 (_, i)
      | i < 5 = 0
      | otherwise = i - 2
    rows = [[m ! (x , y) | x <- [0..n]] | y <- [0..n]]
    cols = [[m ! (x , y) | y <- [0..n]] | x <- [0..n]]

score2 :: Matrix -> Int
score2 m = 3 * length (filter solid (indices m))
  where
    solid (x, y) = and
      [ x > 0, y > 0
      , m ! (x - 1, y) == c
      , m ! (x, y - 1) == c
      , m ! (x - 1, y - 1) == c ]
      where c = m ! (x, y)

score3 :: Matrix -> Int
score3 m = 40 * length found
  where
    (_, (n, _)) = bounds m
    cs = range ((11,11),(n,n))
    pt = [ Dark, Light, Dark, Dark, Dark, Light
         , Dark, Light, Light, Light, Light ]
    pts = [pt, reverse pt]
    found = filter (matches (\x y -> m ! (x, y))) cs
         ++ filter (matches (\x y -> m ! (y, x))) cs
    matches mat (x, y) = map (mat y) [x - 10 .. x] `elem` pts

score4 :: Matrix -> Int
score4 m = 10 * deviation
  where
    (_, (n, _)) = bounds m
    darks = length (filter (== Dark) (elems m))
    total = (n + 1) * (n + 1)
    deviation = truncate (abs (fromIntegral darks * (20 :: Double)
                               / fromIntegral total - 10))

score :: Matrix -> Int
score m = score1 m + score2 m + score3 m + score4 m

formatInfo :: Level -> Int -> [Bit]
formatInfo l k = toBinary 15 $ pats !! (levelIndex l * 8 + k)
  where
    pats :: [Int]
    pats = [ 30660, 29427, 32170, 30877, 26159, 25368, 27713
           , 26998, 21522, 20773, 24188, 23371, 17913, 16590
           , 20375, 19104, 13663, 12392, 16177, 14854, 9396
           , 8579, 11994, 11245, 5769, 5054, 7399, 6608, 1890
           , 597, 3340, 2107]

versionInfo :: Version -> [Bit]
versionInfo v = toBinary 18 $ pats !! (v - 7)
  where
    pats :: [Int]
    pats = [ 31892, 34236, 39577, 42195, 48118, 51042, 55367
           , 58893, 63784, 68472, 70749, 76311, 79154, 84390
           , 87683, 92361, 96236, 102084, 102881, 110507, 110734
           , 117786, 119615, 126325, 127568, 133589, 136944
           , 141498, 145311, 150283, 152622, 158308, 161089, 167017]

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
alignment n = error $ "alignment: invalid version " ++ show n

{-# ANN module "HLint: ignore Use ++" #-}
{-# ANN module "HLint: ignore Use ||" #-}
{-# ANN showMatrix "HLint: ignore Avoid lambda" #-}
{-# ANN score3 "HLint: ignore Use curry" #-}
