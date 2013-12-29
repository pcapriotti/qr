module Types where

type Version = Int
data Mode = Numeric | Alpha | Byte
data Level = L | M | Q | H
  deriving (Eq, Ord, Read, Show, Enum)
