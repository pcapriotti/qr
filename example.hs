module Example where

import Data.Array
import Layout

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
