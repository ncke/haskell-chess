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


best :: Player -> [Game] -> Game
best player games =
  where
    evals' = map evaluate games

search :: Int -> Player -> [Game] -> Game
search depth player games
  | depth == 1   = best evals'
  | otherwise    = search (depth - 1) next'
  where
    evals' = map evaluate games
    best' = max (map score) evals'
    next' = map generateGames games



alphabeta :: Bool -> (Double, Double) -> Double -> (Double, Double)
alphabeta isMax (alpha, beta) score
  | isMax = (max alpha score, beta)
  | otherwise = (alpha, min beta score)


populate2 :: Int -> Player -> (Double, Double) -> Game -> (Double, Double) -> Tree Evaluation
populate2 depth maxPlayer ab game
  | depth == 1   = Tree.Node (eval') []
  | otherwise    = Tree.Node (eval') subtree'
  where
    
    eval' = evaluate game
    score' = score eval'
    ab' = alphabeta (Game.player game == maxPlayer) ab
    games' = generateGames game
    subtree' = map (populate2 (depth - 1)) games'





--search :: Player -> Tree Evaluation -> Game
--search player root =
--  g
--  where g = fmap game root 


main :: IO ()
main = do
  putStrLn(asString board1 White)
