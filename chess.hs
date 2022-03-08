-- Representation of a chess board.

data Player = White | Black deriving (Show, Eq)

type Square = (Int, Int)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

type Piece = (PieceType, Player, Square, Bool)

type Board = [Piece]

-- Chess board manipulation.

location :: Piece -> Square
location (_, _, square, _) = square

owner :: Piece -> Player
owner (_, player, _, _) = player

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
reposition piece square =
  case piece of
    (Pawn, player, orig, flag) -> (Pawn, player, square, isDoubleStep orig square)
    (King, player, _, _) -> (King, player, square, True)
    (pieceType, player, _, _) -> (pieceType, player, square, False)

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
  (King, White, (2, 7), True),  
  (Queen, White, (1, 7), False),
  (Bishop, White, (4, 0), False),
  (King, Black, (0, 3), True) ] 

