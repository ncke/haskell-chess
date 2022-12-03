module Position 
( Position(..)
, Step(..)
, isLegal
, increment
) where

type Position = (Int, Int)

type Step = (Int, Int)

-- determine whether a position has legal coordinates
isLegal :: Position -> Bool
isLegal (x, y)
  | x < 0 = False
  | x > 7 = False
  | y < 0 = False
  | y > 7 = False
  | otherwise = True

-- increment a position by a given step size
increment :: Position -> Step -> Maybe Position
increment (px, py) (dx, dy) =
  case isLegal incremented of
    True -> Just incremented
    False -> Nothing
  where incremented = (px + dx, py + dy)

