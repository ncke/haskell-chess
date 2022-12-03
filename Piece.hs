module Piece 
( PieceKind(..)
, Piece
, kind
, player
, position
) where

import Position
import Player

data PieceKind = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

type Piece = (PieceKind, Player, Position)

kind :: Piece -> PieceKind
kind (kind, player, posn) = kind

player :: Piece -> Player
player (kind, player, posn) = player

position :: Piece -> Position
position (kind, player, posn) = posn