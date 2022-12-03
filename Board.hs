module Board 
( Board
, occupant
, move
, pieces
) where

import Data.List ( find )
import Piece
import Position
import Player

type Board = [Piece]

-- returns true iff the piece is at the position
isAt :: Position -> Piece -> Bool
isAt posn (pi, pl, po) = po == posn

-- returns true iff the piece is at any of the positions
isAts :: [Position] -> Piece -> Bool
isAts posns piece = any (\posn -> isAt posn piece) posns

-- update a piece's position
reposition :: Piece -> Position -> Piece
reposition (piece, player, posn) newPosn = (piece, player, newPosn)

-- find the piece occupying a given position (if any)
occupant :: Board -> Position -> Maybe Piece
occupant b p = find (isAt p) b

-- remove any piece at the given position
remove :: Board -> Position -> Board
remove board posn = filter (\piece -> not (isAt posn piece)) board

-- remove any piece at the given positions
removes :: Board -> [Position] -> Board
removes board posns = filter (\piece -> not (isAts posns piece)) board

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
