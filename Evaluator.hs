module Evaluator where

import Board
import Game
import Piece
import Player

score :: Game -> Double
score game = playersScore game White - playersScore game Black

playersScore :: Game -> Player -> Double
playersScore game player = piecesValue'
  where
    board' = Game.board game
    pieces' = Board.pieces board' player
    piecesValue' = foldr (+) 0 (map pieceValue pieces') 

pieceValue :: Piece -> Double
pieceValue piece =
  case Piece.kind piece of
    Pawn -> 1.0
    Knight -> 3.0
    Bishop -> 3.0
    Rook -> 5.0
    Queen -> 9.0
    King -> 1000000.0
