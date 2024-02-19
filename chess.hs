import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes)

-- Data types
data Piece = Piece PieceColor PieceType deriving (Show, Eq)
data PieceColor = White | Black deriving (Show, Eq)
data PieceType = Pawn | Bishop | Knight | Rook | Queen | King deriving (Show, Eq)

data Square = Square Char Int deriving (Show, Eq)
data Board = Board [(Square, Maybe Piece)] deriving (Show, Eq)
data Pos = Pos Char Int deriving (Show, Eq)
data State = State -- TODO

initialBoard :: Board
initialBoard = Board [
    (Square 'A' 1, Just (Piece White Rook)),
    (Square 'B' 1, Just (Piece White Knight)),
    (Square 'C' 1, Just (Piece White Bishop)),
    (Square 'D' 1, Just (Piece White Queen)),
    (Square 'E' 1, Just (Piece White King)),
    (Square 'F' 1, Just (Piece White Bishop)),
    (Square 'G' 1, Just (Piece White Knight)),
    (Square 'H' 1, Just (Piece White Rook)),
    (Square 'A' 2, Just (Piece White Pawn)),
    (Square 'B' 2, Just (Piece White Pawn)),
    (Square 'C' 2, Just (Piece White Pawn)),
    (Square 'D' 2, Just (Piece White Pawn)),
    (Square 'E' 2, Just (Piece White Pawn)),
    (Square 'F' 2, Just (Piece White Pawn)),
    (Square 'G' 2, Just (Piece White Pawn)),
    (Square 'H' 2, Just (Piece White Pawn)),
    (Square 'A' 8, Just (Piece Black Rook)),
    (Square 'B' 8, Just (Piece Black Knight)),
    (Square 'C' 8, Just (Piece Black Bishop)),
    (Square 'D' 8, Just (Piece Black Queen)),
    (Square 'E' 8, Just (Piece Black King)),
    (Square 'F' 8, Just (Piece Black Bishop)),
    (Square 'G' 8, Just (Piece Black Knight)),
    (Square 'H' 8, Just (Piece Black Rook)),
    (Square 'A' 7, Just (Piece Black Pawn)),
    (Square 'B' 7, Just (Piece Black Pawn)),
    (Square 'C' 7, Just (Piece Black Pawn)),
    (Square 'D' 7, Just (Piece Black Pawn)),
    (Square 'E' 7, Just (Piece Black Pawn)),
    (Square 'F' 7, Just (Piece Black Pawn)),
    (Square 'G' 7, Just (Piece Black Pawn)),
    (Square 'H' 7, Just (Piece Black Pawn))
  ]

prettyBoard :: Board -> String

evalBoard :: Board->Int

-- Can also try guards instead of case
valuePiece :: Piece -> Int
valuePiece (Piece _ pieceType) =
    case pieceType of
    Pawn -> 1
    Knight -> 3
    Bishop -> 3
    Rook -> 5
    Queen -> 9
    King -> 1000