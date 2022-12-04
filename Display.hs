module Display 
( asString
) where

import Board
import Piece
import Player
import Position

asString :: Board -> Player -> String
asString board player =
  concatMap (line board player) rows'
  ++ postfix player
  ++ newline
  where
    rows' = if player == Black then [0..7] else reverse [0..7]
--pref' ++ concatMap (\col -> (repn' row col)) cols' ++ etcs'
line :: Board -> Player -> Int -> String
line board player row =
    pref' ++ concatMap (repn' row) cols' ++ etcs'
  where
    pref' = prefix row
    occu' r c = Board.occupant board (r, c)
    repn' r c = ctrl (occu' r c)  (r, c) ++ pc board (r, c)
    cols' = if player == White then [0..7] else reverse [0..7]
    etcs' = reset ++ newline

prefix :: Int -> String
prefix row = show (row + 1)

letters :: String
letters = "abcdefgh"

postfix :: Player -> String
postfix White = " " ++ letters
postfix Black = " " ++ reverse letters

ctrl :: Maybe Piece -> Position -> String
ctrl piece posn = "\27[" ++ str ++ ";" ++ (bg posn) ++ "m"
  where
    str = case piece of
      Just p -> fg (Piece.player p)
      Nothing -> fg Black

reset :: String
reset = "\27[0m"

bg :: Position -> String
bg (row, col) =
  case mod (row + col) 2 of
    0 -> "44" -- Black
    1 -> "41" -- White

fg :: Player -> String
fg White = "37"
fg Black = "30"

pc :: Board -> Position -> String
pc board posn =
  case piece' of
    Just p -> pieceString p
    Nothing -> " "
  where
    piece' = Board.occupant board posn

newline :: String
newline = "\n"

pieceString :: Piece -> String
pieceString piece =
  case (Piece.player piece, Piece.kind piece) of
    (Black, King)   -> "\9812"
    (Black, Queen)  -> "\9813"
    (Black, Rook)   -> "\9814"
    (Black, Bishop) -> "\9815"
    (Black, Knight) -> "\9816"
    (Black, Pawn)   -> "\9817"
    (White, King)   -> "\9818"
    (White, Queen)  -> "\9819"
    (White, Rook)   -> "\9820"
    (White, Bishop) -> "\9821"
    (White, Knight) -> "\9822"
    (White, Pawn)   -> "\9823"
