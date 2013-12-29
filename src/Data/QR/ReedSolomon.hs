module Data.QR.ReedSolomon
  ( errorWords
  ) where

import Data.Bits
import Data.Word

raise :: Word8 -> Word8
raise w | w .&. 0x80 == 0 = w'
        | otherwise       = w' `xor` 285
  where w' = shiftL w 1

mult :: Word8 -> Word8 -> Word8
mult = go 0
  where
    go a 0 _ = a
    go a x y = go (a `xor` r) x' y'
      where
        x' = shiftR x 1
        y' = raise y
        r = if x .&. 1 == 0 then 0 else y

data Poly a = Poly
  { incDegree :: Int
  , coeff :: [a] }
  deriving (Eq, Read, Show)

mkPoly :: (Eq a, Num a) => [a] -> Poly a
mkPoly xs = Poly (length xs') xs'
  where
    xs' = dropWhile (== 0) xs

polyMod :: Poly Word8 -> Poly Word8 -> Poly Word8
polyMod p q
  | k < 0     = p
  | null as = mkPoly []
  | otherwise = polyMod (mkPoly xs) q
  where
    k = incDegree p - incDegree q
    bs = coeff q ++ replicate k 0
    as = coeff p
    a = head as
    bs' = map (mult a) bs
    xs = zipWith xor as bs'

polyAdd :: Poly Word8 -> Poly Word8 -> Poly Word8
polyAdd p q = mkPoly (zipWith xor as bs)
  where
    d = incDegree p `max` incDegree q
    as = replicate (d - incDegree p) 0 ++ coeff p
    bs = replicate (d - incDegree q) 0 ++ coeff q

polyMult' :: Word8 -> Poly Word8 -> Poly Word8
polyMult' 0 _ = mkPoly []
polyMult' a p = Poly (incDegree p) (map (mult a) (coeff p))

polyRaise :: Poly Word8 -> Poly Word8
polyRaise p = Poly (incDegree p + 1) (coeff p ++ [0])

polyMult :: Poly Word8 -> Poly Word8 -> Poly Word8
polyMult p = go (mkPoly []) (reverse (coeff p))
  where
    go rs [] _ = rs
    go rs (a : as) q =
      go (polyAdd rs (polyMult' a q))
         as (polyRaise q)

gen :: Int -> Poly Word8
gen n | n < 1 = error "invalid generator"
gen n = foldr1 polyMult [mkPoly [1,a] | a <- pows]
  where
    pows = take n $ iterate (mult 2) 1

errorPoly :: Int -> Poly Word8 -> Poly Word8
errorPoly n p = polyMod p' (gen n)
  where
    p' = Poly (incDegree p + n) (coeff p ++ replicate n 0)

errorWords :: Int -> [Word8] -> [Word8]
errorWords n = coeff . errorPoly n . mkPoly
