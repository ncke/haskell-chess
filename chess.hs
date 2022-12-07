module Main where

import Board
import Evaluator
import Piece
import MoveGenerator
import Player
import Game
import Display
import Tree

board1 :: Board
board1 = [Piece.create King White (3,0), Piece.create Queen Black (4,1)]

board2 :: Board
board2 = [Piece.create King White (3,3)]

game1 :: Game
game1 = Game.create board1 White

game2 :: Game
game2 = Game.create board2 Black



play :: Game -> Game
play game =
  game

populate :: Int -> Game -> Tree Evaluation
populate depth game =
  | depth == 1 = Tree.Node (evaluate game) []
  | otherwise = Tree.Node (evaluate game) subtree
  where subtree = map (populate (depth - 1)) (generateGames game)

--search :: Player -> Tree Evaluation -> Game
--search player root =
--  g
--  where g = fmap game root 


main :: IO ()
main = do
  putStrLn(asString board1 White)
