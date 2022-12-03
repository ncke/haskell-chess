module Piece 
( PieceKind(..)
, Piece
, kind
, player
, position
, isAt
, isAts
) where

import Player
import Position

data PieceKind = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

type Piece = (PieceKind, Player, Position)

-- returns the kind of the piece
kind :: Piece -> PieceKind
kind (kind, player, posn) = kind

-- returns the player that owns the piece
player :: Piece -> Player
player (kind, player, posn) = player

-- returns the position at which the piece is located
position :: Piece -> Position
position (kind, player, posn) = posn

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
