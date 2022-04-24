data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Player = White | Black deriving (Show, Eq)

type Square = Maybe (Piece, Player)

type Board = [Square]

data CastleSide = Kingside | Queenside deriving (Show, Eq)

type Castleables = [CastleSide]


