module Pattern
( Pattern
, pattern
, recurses
, steps
) where

import Piece
import Position

type Pattern = (Bool, [Step])

steps :: Pattern -> [Step]
steps (_, steps) = steps

recurses :: Pattern -> Bool
recurses (recs, _) = recs

pattern :: PieceKind -> Pattern
pattern kind =
  case kind of
    Knight -> (False, knightSteps)
    Bishop -> (True, diagonalSteps)
    Rook   -> (True, parallelSteps)
    Queen  -> (True, parallelSteps ++ diagonalSteps)
    King   -> (False, parallelSteps ++ diagonalSteps)

parallelSteps :: [Step]
parallelSteps = [ (1,0), (-1,0), (0,1), (0,-1) ]

diagonalSteps :: [Step]
diagonalSteps = [ (1,1), (-1,1), (1,-1), (-1,-1) ]

knightSteps :: [Step]
knightSteps = [ (1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2) ]
