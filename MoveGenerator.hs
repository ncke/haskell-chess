module MoveGenerator 
( generateGames
) where

import Board
import Game
import Pattern
import Piece
import Player
import Position

generateGames :: Game -> [Game]
generateGames game = 
  map (Game.move game) boards'
  where 
    board' = Game.board game
    turn' = Game.turn game
    pieces' = Board.pieces board' turn'
    boards' = concatMap (generateBoards game) pieces'

generateBoards :: Game -> Piece -> [Board]
generateBoards game piece =
  map (Board.move board' origin') posns'
  where
    board' = Game.board game
    origin' = Piece.position piece
    posns' = generatePieceMoves game piece

generatePieceMoves :: Game -> Piece -> [Position]
generatePieceMoves game piece =
  case kind' of
    Pawn -> []
    other -> generatePatternedPieceMoves game piece (pattern kind')
  where
    kind' = Piece.kind piece

generatePatternedPieceMoves :: Game -> Piece -> Pattern -> [Position]
generatePatternedPieceMoves game piece pattern =
  applySteps board' piece' posn' pattern
  where 
    board' = Game.board game
    piece' = Piece.player piece
    posn' = Piece.position piece

applyStep :: Board -> Player -> Position -> Bool -> Step -> [Position]
applyStep board player posn recurse step =
  case Position.increment posn step of
    Just next -> 
      if Board.hasSamePlayer board player next then []
      else if Board.hasOppoPlayer board player next || not recurse then [next]
      else next : applyStep board player next recurse step
    Nothing -> []

applySteps :: Board -> Player -> Position -> Pattern -> [Position]
applySteps board player posn pattern =
  concatMap (applyStep board player posn recurses') steps'
  where
    recurses' = Pattern.recurses pattern
    steps' = Pattern.steps pattern
