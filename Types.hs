module Types where

type Version = Int
data Mode = Numeric | Alpha | Byte
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
