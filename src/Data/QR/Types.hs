module Data.QR.Types where

import Data.List
import Data.Word

type Version = Int
data Mode = Numeric | Alpha | Byte
  deriving (Eq, Ord, Read, Show, Enum)
data Level = L | M | Q | H
  deriving (Eq, Ord, Read, Show, Enum)

levelIndex :: Level -> Int
levelIndex L = 0
levelIndex M = 1
levelIndex Q = 2
levelIndex H = 3

data Bit = Z | O
  deriving (Eq, Ord, Read)

instance Show Bit where
  show Z = "0"
  show O = "1"

toBinary :: Integral a => Int -> a -> [Bit]
toBinary = go []
  where
    go bs n 0 = replicate n Z ++ bs
    go bs n x = case divMod x 2 of
      (x', b) -> go ((if b == 0 then Z else O) : bs) (n - 1) x'

toWords :: [Bit] -> [Word8]
toWords = map toWord . chunksOf 8

toWord :: [Bit] -> Word8
toWord = foldl' (\x b -> x + x + (if b == Z then 0 else 1)) 0

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = case splitAt n xs of
  (xs1, xs') -> xs1 : chunksOf n xs'
