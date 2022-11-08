import Data.List

data PieceKind = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Player = White | Black deriving (Show, Eq)

type Position = (Int, Int)

type Piece = (PieceKind, Player, Position)

type Board = [Piece]

isLegal :: Position -> Bool
isLegal (x, y)
  | x < 0 = False
  | x > 7 = False
  | y < 0 = False
  | y > 7 = False
  | otherwise = True

isAt :: Position -> Piece -> Bool
isAt posn (pi, pl, po) = po == posn 

reposition :: Piece -> Position -> Piece
reposition (piece, player, posn) newPosn = (piece, player, newPosn)

occupant :: Board -> Position -> Maybe Piece
occupant b p = find (isAt p) b

remove :: Board -> Position -> Board
remove board posn = filter (\piece -> not (isAt posn piece)) board

place :: Board -> Piece -> Board
place board piece = piece : board


move :: Board -> Position -> Position -> Board
move board oldPosn newPosn =
  case occupant board oldPosn of
    Just piece -> (reposition piece newPosn) : (remove board oldPosn) 
    Nothing -> board



board1 :: Board
board1 = [(King, White, (3, 0)), (Queen, Black, (4,1))]



