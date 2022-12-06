module Evaluator 
( Evaluation
, game
, score
, evaluate
) where

import Board
import Game
import Piece
import Player
import Position

data Evaluation = Evaluation 
  { game :: Game
  , score :: Double
  } deriving (Show)

evaluate :: Game -> Evaluation
evaluate game =
  Evaluation { game = game, score = score }
  where score = playersScore game White - playersScore game Black

playersScore :: Game -> Player -> Double
playersScore game player = value' + bishops' + central'
  where
    board' = Game.board game
    pieces' = Board.pieces board' player
    value' = foldr (+) 0 (map pieceValue pieces')
    bishops' = twoBishops pieces'
    central' = centrality pieces'

pieceValue :: Piece -> Double
pieceValue piece =
  case Piece.kind piece of
    Pawn -> 1.0
    Knight -> 3.0
    Bishop -> 3.0
    Rook -> 5.0
    Queen -> 9.0
    King -> 1000000.0

twoBishops :: [Piece] -> Double
twoBishops pieces = if length bishops == 2 then 0.5 else 0.0
  where bishops = filter (Bishop ==) (map kind pieces)

centrality :: [Piece] -> Double
centrality pieces = 0.15 * fromIntegral centre + 0.05 * fromIntegral surround
  where
    posns = map position pieces
    centre = length (filter (inside (4,4) (5,5)) posns)
    surround = length (filter (inside (3,3) (6,6)) posns) - centre
