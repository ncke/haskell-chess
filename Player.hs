module Player
( Player(..)
, opponent
) where

data Player = White | Black deriving (Show, Eq)

opponent :: Player -> Player
opponent White = Black
opponent Black = White
