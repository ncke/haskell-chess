module MoveGenerator 
( gameGenerate
) where

import Piece
import Board
import Game
import Position
import Player


gameGenerate :: Game -> [Game]
gameGenerate game = 
  map (Game.move game) boards'
  where 
    board' = Game.board game
    turn' = Game.turn game
    pieces' = Board.pieces board' turn'
    boards' = concatMap (\piece -> boardGenerate game piece) pieces'

boardGenerate :: Game -> Piece -> [Board]
boardGenerate game piece =
  map (\posn -> Board.move board' origin' posn) posns'
  where
    board' = Game.board game
    origin' = Piece.position piece
    posns' = pieceGenerate game piece

pieceGenerate :: Game -> Piece -> [Position]
pieceGenerate game piece =
  case kind of
    Pawn -> []
    other -> patternGenerate game piece (pattern kind)
  where
    kind = Piece.kind piece

pattern :: PieceKind -> ([Step], Bool)
pattern kind =
  case kind of
    Knight -> (knightSteps, False)
    Bishop -> (diagonalSteps, True)
    Rook -> (parallelSteps, True)
    Queen -> (parallelSteps ++ diagonalSteps, True)
    King -> (parallelSteps ++ diagonalSteps, False)



patternGenerate :: Game -> Piece -> ([Step], Bool) -> [Position]
patternGenerate game piece (steps, recurse) =
  applySteps board' piece' posn' steps recurse
  where 
    board' = Game.board game
    piece' = Piece.player piece
    posn' = Piece.position piece



parallelSteps :: [Step]
parallelSteps = [ (1,0), (-1,0), (0,1), (-1,0) ]

diagonalSteps :: [Step]
diagonalSteps = [ (1,1), (-1,1), (1,-1), (-1,-1) ]

knightSteps :: [Step]
knightSteps = [ (1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2) ]




hasSamePlayer :: Board -> Player -> Position -> Bool
hasSamePlayer board player posn =
  case occupant board posn of
    Just occupier -> player == Piece.player occupier
    Nothing -> False

hasOpposingPlayer :: Board -> Player -> Position -> Bool
hasOpposingPlayer board player posn =
  case occupant board posn of
    Just occupier -> player /= Piece.player occupier
    Nothing -> False

applyStep :: Board -> Player -> Position -> Step -> Bool -> [Position]
applyStep board player posn step recurse =
  case once of
    Just next -> 
      if hasSamePlayer board player next then []
      else if hasOpposingPlayer board player next || not recurse then [next]
      else next : applyStep board player next step recurse
    Nothing -> []
  where once = Position.increment posn step

applySteps :: Board -> Player -> Position -> [Step] -> Bool -> [Position]
applySteps board player posn steps recurse =
    concatMap (\step -> applyStep board player posn step recurse) steps
