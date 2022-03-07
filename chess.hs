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

owner :: Piece -> Player
owner (Pawn player _ _) = player
owner (King player _ _) = player
owner (Queen player _) = player
owner (Rook player _) = player
owner (Bishop player _) = player
owner (Knight player _) = player

occupant :: Board -> Square -> Maybe Player
occupant board square =
  let ps = filter (\ piece -> (location piece) == square) board in
  case ps of
    [] -> Nothing
    _  -> Just (owner (head ps))

remove :: Board -> [Square] -> Board
remove board squares =
  filter (\ piece -> elem (location piece) squares) board

isDoubleStep :: Square -> Square -> Bool
isDoubleStep (_, yorig) (_, ydest) = abs(ydest - yorig) == 2

reposition :: Piece -> Square -> Piece
reposition (Pawn player orig flag) square = Pawn player square (isDoubleStep orig square) 
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

