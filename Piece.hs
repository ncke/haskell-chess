module Piece 
( PieceKind(..)
, Piece
, kind
, player
, position
, isAt
, isAts
, reposition
, create
) where

import Player
import Position

data PieceKind = 
  Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Piece = Piece { kind :: PieceKind
                   , player :: Player
                   , position :: Position
                   } deriving (Show)

-- returns true iff the piece is at the position
isAt :: Piece -> Position -> Bool
isAt piece posn = position piece == posn

-- returns true iff the piece is at any of the positions
isAts :: Piece -> [Position] -> Bool
isAts piece posns =
  case posns of
    [] -> False
    (posn : posns) -> if posn == posn' then True else isAts piece posns
  where
    posn' = position piece

-- returns a piece that is repositioned to the given position
reposition :: Piece -> Position -> Piece
reposition piece posn = 
  Piece { kind = kind piece
        , player = player piece
        , position = posn 
        }

-- create a piece
create :: PieceKind -> Player -> Position -> Piece
create kind player posn =
  Piece { kind = kind
        , player = player
        , position = posn
        }
