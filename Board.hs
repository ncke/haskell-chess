module Board 
( Board
, occupant
, move
, pieces
, hasSamePlayer
, hasOppoPlayer
) where

import Piece
import Position
import Player

type Board = [Piece]

-- find the piece occupying a given position (if any)
occupant :: Board -> Position -> Maybe Piece
occupant board posn =
  case board of
    [] -> Nothing
    (p : ps) -> if Piece.isAt p posn then Just p else occupant ps posn 

-- remove any piece at the given position
remove :: Board -> Position -> Board
remove board posn = filter (\piece -> not (isAt piece posn)) board

-- remove any piece at the given positions
removes :: Board -> [Position] -> Board
removes board posns = filter (\piece -> not (isAts piece posns)) board

-- move a piece to a new position and remove any captured piece
move :: Board -> Position -> Position -> Board
move board oldPosn newPosn =
  case occupant board oldPosn of
    Just piece -> (reposition piece newPosn) : (removes board affected) 
    Nothing -> board
  where
    affected = [oldPosn, newPosn]

-- find all pieces belonging to the given player
pieces :: Board -> Player -> [Piece]
pieces board player = filter (\piece -> Piece.player piece == player) board

-- True iff the given player has a piece at the given position
hasSamePlayer :: Board -> Player -> Position -> Bool
hasSamePlayer board player posn =
  case occupant board posn of
    Just occupier -> player == Piece.player occupier
    Nothing -> False

-- True iff the given player's opponent has a piece at the given position
hasOppoPlayer :: Board -> Player -> Position -> Bool
hasOppoPlayer board player posn =
  case occupant board posn of
    Just occupier -> player /= Piece.player occupier
    Nothing -> False