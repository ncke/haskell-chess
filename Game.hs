module Game
( Game
, turn
, board
, Game.move
, Game.create
) where

import Board
import Piece
import Player

data Game = Game { board :: Board
                 , turn :: Player
                 } deriving (Show)

-- returns a game after a move has been played to the given board
move :: Game -> Board -> Game
move game board =
  Game { board = board
       , turn = opponent (turn game)
       }

-- create a game
create :: Board -> Player -> Game
create board player =
  Game { board = board
       , turn = player
       }