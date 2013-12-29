module Example where

import Data.Array
import Data.Word
import Layout
import Encode
import Grouping
import Types
import ReedSolomon

c :: Char -> Module
c ' ' = Light
c _   = Dark

ex0 :: Matrix
ex0 = listArray ((0, 0), (20, 20)) (map c txt)
  where
    txt = concat
      [ "xxxxxxx xx    xxxxxxx"
      , "x     x x  x  x     x"
      , "x xxx x x  xx x xxx x"
      , "x xxx x x     x xxx x"
      , "x xxx x x x   x xxx x"
      , "x     x   x   x     x"
      , "xxxxxxx x x x xxxxxxx"
      , "        x            "
      , " xx x xx    x x xxxxx"
      , " x      xxxx    x   x"
      , "  xx xxx xx   x xx   "
      , " xx xx x  xx x x xxx "
      , "x   x x x xxx xxx x x"
      , "        xx x  x   x x"
      , "xxxxxxx x x    x xx  "
      , "x     x  x xx xx x   "
      , "x xxx x x x   xxxxxxx"
      , "x xxx x  x x x x   x "
      , "x xxx x x   xxxx x  x"
      , "x     x x xx x   x xx"
      , "xxxxxxx     xxxx    x" ]

ex1 :: [Word8]
ex1 = map toWord bits
  where
    bits = [ [ Z, O, Z, Z, Z, Z, O, O]
           , [ Z, O, Z, O, Z, O, Z, O]
           , [ Z, O, Z, Z, Z, O, O, Z]
           , [ O, Z, Z, Z, Z, O, O, Z]
           , [ Z, O, Z, O, Z, O, O, O]
           , [ Z, Z, O, Z, Z, O, O, Z]
           , [ Z, O, Z, O, Z, O, Z, O]
           , [ O, O, Z, Z, Z, Z, O, Z]
           , [ Z, O, O, O, Z, O, O, O]
           , [ Z, Z, O, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, Z]
           , [ Z, Z, Z, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, Z]
           , [ Z, O, O, Z, Z, O, O, O]
           , [ Z, Z, O, Z, Z, O, O, Z]
           , [ O, O, O, O, Z, O, O, Z]
           , [ O, O, O, O, Z, O, O, Z]
           , [ Z, O, Z, Z, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, O]
           , [ Z, O, O, O, Z, O, O, Z]
           , [ O, Z, Z, Z, Z, O, O, Z]
           , [ O, O, O, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, O]
           , [ Z, Z, O, Z, Z, O, O, Z]
           , [ Z, O, Z, O, Z, O, O, Z]
           , [ Z, Z, Z, O, Z, O, O, Z]
           , [ O, O, Z, Z, Z, O, O, Z]
           , [ O, O, Z, Z, Z, O, O, O]
           , [ O, Z, Z, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, Z]
           , [ O, Z, O, O, Z, O, O, Z]
           , [ O, O, O, Z, Z, O, O, Z]
           , [ O, O, O, O, Z, O, O, O]
           , [ Z, O, O, O, Z, O, O, O]
           , [ Z, Z, O, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, O]
           , [ Z, O, O, O, Z, O, O, Z]
           , [ O, Z, Z, Z, Z, O, O, Z]
           , [ Z, O, Z, O, Z, O, O, O]
           , [ Z, Z, O, Z, Z, O, O, Z]
           , [ Z, O, Z, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, Z]
           , [ O, Z, Z, Z, Z, O, O, Z]
           , [ O, Z, Z, O, Z, O, O, O]
           , [ Z, Z, O, O, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, O]
           , [ Z, O, Z, Z, Z, O, O, Z]
           , [ O, O, O, O, Z, O, O, O]
           , [ Z, O, O, O, Z, O, O, Z]
           , [ Z, O, Z, O, Z, O, O, Z]
           , [ O, O, Z, Z, Z, Z, O, Z]
           , [ Z, Z, Z, Z, Z, O, O, Z]
           , [ O, Z, Z, O, Z, O, O, O]
           , [ Z, Z, O, O, Z, Z, O, Z]
           , [ Z, Z, Z, O, Z, Z, Z, Z]
           , [ O, O, O, Z, O, O, Z, Z]
           , [ Z, Z, Z, O, Z, Z, Z, O]
           , [ O, O, O, Z, O, O, Z, Z]
           , [ Z, Z, Z, O, Z, Z, Z, O]
           , [ O, O, O, Z, O, O, Z, Z]
           , [ Z, Z, Z, O, Z, Z, Z, O]
           , [ O, O, O, Z, O, O, Z, Z] ]
