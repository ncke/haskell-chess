-- Representation of a chess board.

type Pawn = (Bool)      -- true if vulnerable to en passant
type King = (Bool)      -- true if has moved so cannot later castle
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Player = White | Black deriving (Show, Eq)
type Piece = (Player, PieceType, Int, Int)
type Board = [Piece]

opponent :: Player -> Player
opponent White = Black
opponent Black = White

pieces :: Board -> Player -> [Piece]
pieces board player = filter (\ (p, t, x, y) -> p == player) board 

locate :: Board -> Int -> Int -> [Piece]
locate board x y = filter (\ (p, t, px, py) -> x == px && y == py) board

remove :: Board -> Int -> Int -> Board
remove board x y = filter (\ (p, t, px, py) -> not (x == px && y == py)) board
 
reposition :: Piece -> Int -> Int -> Piece
reposition (p, t, px, py) x y = (p, t, x, y)

move :: Board -> Int -> Int -> Int -> Int -> Board
move board xfrom yfrom xto yto = remove 
  ((reposition (head (locate board xfrom yfrom)) xto yto) : board) 
  xfrom 
  yfrom

isLegalCoord :: Int -> Int -> Bool
isLegalCoord x y = x >= 0 && x <= 7 && y >= 0 && y <= 7

isIllegalCoord :: Int -> Int -> Bool
isIllegalCoord x y = not (isLegalCoord x y)

isUnoccupied :: Board -> Int -> Int -> Bool
isUnoccupied board x y =
  locate board x y == []

isFriendlyOccupied :: Board -> Player -> Int -> Int -> Bool
isFriendlyOccupied board player x y = let ps = locate board x y in
  if ps == [] then False
  else let (p, t, px, py) = head(ps) in p == player

isOpponentOccupied :: Board -> Player -> Int -> Int -> Bool
isOpponentOccupied board player x y = let ps = locate board x y in
  if ps == [] then False
  else let (p, t, px, py) = head(ps) in p == opponent player

ray :: [(Int, Int)] -> Board -> Player-> Int -> Int -> Int -> Int -> [(Int, Int)]
ray accum board player xorig yorig dx dy =
  if isFriendlyOccupied board player (xorig + dx) (yorig + dy) then accum
  else if isOpponentOccupied board player (xorig + dx) (yorig + dy) then (xorig + dx, yorig + dy) : accum
  else if isIllegalCoord (xorig + dx) (yorig + dy) then accum
  else (xorig + dx, yorig + dy) : ray accum board player (xorig + dx) (yorig + dy) dx dy

knightPattern :: [(Int, Int)] 
knightPattern = [(-2, 1), (-1, 2), (1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1)]


isLegalCoord :: (Int, Int) -> Bool
isLegalCoord x y = x >= 0 && x <= 7 && y >= 0 && y <= 7

offset :: (Int, Int) -> (Int, Int) -> (Int, Int)
offset (xorig, yorig) (xvec, yvec) = (xorig + xvec, yorig + yvec) 

applyPattern :: Board -> Player -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
applyPattern board player origin pattern =
  filter isLegalCoord . map (\ vector -> offset origin vector)

knightMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
knightMoves board player x y =
  (filter (\ (px, py) -> not(isFriendlyOccupied board player px py))
  . filter (\ (px, py) -> isLegalCoord px py)
  . map (\ (dx, dy) -> (x + dx, y + dy))) 
  knightPattern

-- knightMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
-- knightMoves board player x y =
--   filter (\ (px, py) -> not(isFriendlyOccupied board player px py))
--   (filter (\ (px, py) -> isLegalCoord px py)
--   (map (\ (dx, dy) -> (x + dx, y + dy)) knightPattern))


bishopMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
bishopMoves board player x y =
  ray [] board player x y 1 1
  ++ ray [] board player x y (-1) 1
  ++ ray [] board player x y 1 (-1)
  ++ ray [] board player x y (-1) (-1)

rookMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
rookMoves board player x y =
  ray [] board player x y 0 1
  ++ ray [] board player x y 0 (-1)
  ++ ray [] board player x y 1 0
  ++ ray [] board player x y (-1) 0

queenMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
queenMoves board player x y =
  rookMoves board player x y
  ++ bishopMoves board player x y

pawnAdvancePattern :: Player -> [(Int, Int)]
pawnAdvancePattern player =
  case player of
    White -> [(0, 1)]
    Black -> [(0, -1)]

pawnCapturePattern :: Player -> [(Int, Int)]
pawnCapturePattern player =
  case player of
    White -> [(-1, 1), (1, 1)]
    Black -> [(-1, -1), (1, -1)]

pawnCapture :: Board -> Player -> Int -> Int -> [(Int, Int)]
pawnCapture board player x y =
  filter (\ (px, py) -> isOpponentOccupied board player px py)
  (filter (\ (px, py) -> isLegalCoord px py)
  (map (\ (dx, dy) -> (x + dx, y + dy)) (pawnCapturePattern player)))

pawnAdvance :: Board -> Player -> Int -> Int -> [(Int, Int)]
pawnAdvance board player x y =
  filter (\ (px, py) -> isUnoccupied board px py)
  (filter (\ (px, py) -> isLegalCoord px py)
  (map (\ (dx, dy) -> (x + dx, y + dy)) (pawnAdvancePattern player)))

  

pawnMoves :: Board -> Player -> Int -> Int -> [(Int, Int)]
pawnMoves board player x y =
  pawnAdvance board player x y
  ++ pawnCapture board player x y

-- Mate in one is available.
problem1 :: Board
problem1 = [ 
  (White, King, 2, 7), 
  (White, Queen, 1,7),
  (White, Bishop, 4, 0),
  (Black, King, 0, 3) ] 

