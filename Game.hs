module Game
( Game(..)
, turn
, board
, Game.move
) where

import Board
import Piece
import Player

type Game = (Board, Player)

turn :: Game -> Player
turn (board, player) = player

board :: Game -> Board
board (board, player) = board

move :: Game -> Board -> Game
move (old, player) board = (board, opponent player)
