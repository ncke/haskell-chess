module MovePattern
( MovePattern
, movePattern
, recurses
, steps
) where

import Piece
import Position

data MovePattern = MovePattern { recurses :: Bool
                               , steps :: [Step]
                               }

movePattern :: PieceKind -> MovePattern
movePattern kind =
  case kind of
    Knight -> MovePattern { recurses = False, steps = knight }
    Bishop -> MovePattern { recurses = True, steps = diagonal }
    Rook   -> MovePattern { recurses = True, steps = parallel }
    Queen  -> MovePattern { recurses = True, steps = parallel ++ diagonal }
    King   -> MovePattern { recurses = False, steps = parallel ++ diagonal }

parallel :: [Step]
parallel = [ (1,0), (-1,0), (0,1), (0,-1) ]

diagonal :: [Step]
diagonal = [ (1,1), (-1,1), (1,-1), (-1,-1) ]

knight :: [Step]
knight = [ (1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2) ]
