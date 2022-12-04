module Main where

import Board
import Piece
import MoveGenerator
import Player
import Game
import Display

board1 :: Board
board1 = [Piece.create King White (3, 0), Piece.create Queen Black (4,1)]

board2 :: Board
board2 = [Piece.create King White (3,3)]

game1 :: Game
game1 = Game.create board1 White

game2 :: Game
game2 = Game.create board2 Black

main :: IO ()
main = do
  putStrLn(asString board1 White)
