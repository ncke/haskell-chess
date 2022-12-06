module Game
( Game
, turn
, player
, board
, Game.move
, Game.create
) where

import Board
import Player

data Game = Game { board :: Board
                 , player :: Player
                 } deriving (Show)

-- returns a game after a move has been played to the given board
move :: Game -> Player -> Board -> Game
move game player board =
  Game { board = board
       , player = player
       }

-- create a game
create :: Board -> Player -> Game
create board player =
  Game { board = board
       , player = player
       }

-- the player next to move
turn :: Game -> Player
turn game = opponent (player game)