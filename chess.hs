module Main where

import Board
import Piece
import MoveGenerator
import Player
import Game
import Display

board1 :: Board
board1 = [create King White (3, 0), create Queen Black (4,1)]

board2 :: Board
board2 = [create King White (3,3)]

game1 :: Game
game1 = (board2, White)

game2 :: Game
game2 = (board2, Black)

main :: IO ()
main = do
  putStrLn(asString board1 White)
