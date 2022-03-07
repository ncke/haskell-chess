-- Representation of a chess board.

data Player = White | Black deriving (Show, Eq)

type Square = (Int, Int)

data Piece = 
    Pawn Player Square Bool 
  | Knight Player Square
  | Bishop Player Square
  | Rook Player Square
  | Queen Player Square
  | King Player Square Bool
  deriving (Show, Eq)

type Board = [Piece]

-- Chess board manipulation.

location :: Piece -> Square
location (Pawn _ square _) = square
location (King _ square _) = square
location (Queen _ square) = square
location (Rook _ square) = square
location (Bishop _ square) = square
location (Knight _ square) = square

remove :: Board -> [Square] -> Board
remove board squares =
  filter (\ piece -> (location piece) elem squares) board

isTwoStep :: Square -> Square -> Bool
isTwoStep (_, yorig) (_, ydest) = abs(ydest - yorig) == 2

reposition :: Piece -> Square -> Piece
reposition (Pawn player orig flag) square = Pawn player square (isTwoStep orig square) 
reposition (King player _ _) square = King player square True
reposition (Queen player _) square = Queen player square
reposition (Rook player _) square = Rook player square
reposition (Bishop player _) square = Bishop player square
reposition (Knight player _) square = Knight player square

move :: Board -> Piece -> Square -> Board
move board piece square =
  (reposition piece square) : remove board [location piece, square]

-- Players

opponent :: Player -> Player
opponent White = Black
opponent Black = White



-- Mate in one is available.
problem1 :: Board
problem1 = [ 
  King White (2, 7) True,  
  Queen White (1, 7),
  Bishop White (4, 0),
  King Black (0, 3) True ] 

