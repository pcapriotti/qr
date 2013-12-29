module Types where

type Version = Int

data Level = L | M | Q | H
  deriving (Eq, Ord, Read, Show, Enum)
