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
    pieces' = Game.pieces game player
    piecesValue' = foldr (+) 0 (map pieceValue pieces') 



pieceValue :: Piece -> Double
pieceValue piece =
  case Piece.kind piece of
    Pawn -> 1.0

