-- Representation of a chess board.

data Player = White | Black deriving (Show, Eq)

type Square = (Int, Int)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

type Piece = (PieceType, Player, Square, Bool)

type Board = [Piece]

-- Squares.

occupant :: Board -> Square -> Maybe Piece
occupant board square =
  let ps = filter (\ piece -> (location piece) == square) board in
  case ps of
    [] -> Nothing
    _  -> Just (head ps)

isLegal :: Square -> Bool
isLegal (x, y) = x >= 0 && y >= 0 && x <= 7 && y <= 7

-- Chess board manipulation.

location :: Piece -> Square
location (_, _, square, _) = square

remove :: Board -> [Square] -> Board
remove board squares =
  filter (\ piece -> not (elem (location piece) squares)) board

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

-- Players.

owner :: Piece -> Player
owner (_, player, _, _) = player

opponent :: Player -> Player
opponent White = Black
opponent Black = White

pieces :: Board -> Player -> [Piece]
pieces board player =
  filter (\ piece -> (owner piece) == player) board

-- Move generation.

ray :: Board -> Player -> Square -> (Int, Int) -> [Square]
ray board player (x, y) (dx, dy) =
  let sq = (x + dx, y + dy) in
  if not(isLegal sq) then []
  else case occupant board sq of
    Nothing -> sq : ray board player sq (dx, dy)
    Just piece -> if owner(piece) == player then [] 
                  else if owner piece == opponent player then [sq] 
                  else sq : ray board player sq (dx, dy)

knightVectors = [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]

knightMoves :: Board -> Player -> Square -> [Square]
knightMoves board player (x, y) =
  filter isLegal
  (map (\ (dx, dy) -> (x + dx, y + dy)) knightVectors)

diagonalRays = [(1, 1), (-1, 1), (-1, -1), (1, -1)]

bishopMoves :: Board -> Player -> Square -> [Square]
bishopMoves board player square =
  concat (map (ray board player square) diagonalRays)

perpendicularRays = [(0, 1), (1, 0), (0, -1), (-1, 0)]

rookMoves :: Board -> Player -> Square -> [Square]
rookMoves board player square =
  concat (map (ray board player square) perpendicularRays)

queenMoves :: Board -> Player -> Square -> [Square]
queenMoves board player square =
  concat (map (ray board player square) (concat [perpendicularRays, diagonalRays]))

pieceMoves :: Board -> Piece -> [Board]
pieceMoves board (Bishop, player, square, flag) = map (move board (Bishop, player, square, flag)) (bishopMoves board player square)

moves :: Board -> Player -> [Board]
moves board player =
  concat (map (pieceMoves board) (pieces board player))

-- Mate in one is available.
problem1 :: Board
problem1 = [ 
  (King, White, (2, 7), True),  
  (Queen, White, (1, 7), False),
  (Bishop, White, (4, 0), False),
  (King, Black, (0, 3), True) ] 

