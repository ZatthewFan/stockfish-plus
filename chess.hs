import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes)

-- Data types
data Piece = Piece PieceColor PieceType deriving (Show, Eq)
data PieceColor = White | Black deriving (Show, Eq)
data PieceType = Pawn | Bishop | Knight | Rook | Queen | King deriving (Show, Eq)

data Pos = Pos Char Int deriving (Show, Eq)
data Square = Square Pos (Maybe Piece) deriving (Show, Eq)
data Board = Board [Square] deriving (Show, Eq)
data State = State { gameState :: Board } deriving (Show, Eq)

initialBoard :: Board
initialBoard = Board [
    Square (Pos 'A' 1) (Just (Piece White Rook)),
    Square (Pos 'B' 1) (Just(Piece White Knight)),
    Square (Pos 'C' 1) (Just (Piece White Bishop)),
    Square (Pos 'D' 1) (Just (Piece White Queen)),
    Square (Pos 'E' 1) (Just (Piece White King)),
    Square (Pos 'F' 1) (Just (Piece White Bishop)),
    Square (Pos 'G' 1) (Just (Piece White Knight)),
    Square (Pos 'H' 1) (Just (Piece White Rook)),
    Square (Pos 'A' 2) (Just (Piece White Pawn)),
    Square (Pos 'B' 2) (Just (Piece White Pawn)),
    Square (Pos 'C' 2) (Just (Piece White Pawn)),
    Square (Pos 'D' 2) (Just (Piece White Pawn)),
    Square (Pos 'E' 2) (Just (Piece White Pawn)),
    Square (Pos 'F' 2) (Just (Piece White Pawn)),
    Square (Pos 'G' 2) (Just (Piece White Pawn)),
    Square (Pos 'H' 2) (Just (Piece White Pawn)),
    Square (Pos 'A' 8) (Just (Piece Black Rook)),
    Square (Pos 'B' 8) (Just (Piece Black Knight)),
    Square (Pos 'C' 8) (Just (Piece Black Bishop)),
    Square (Pos 'D' 8) (Just (Piece Black Queen)),
    Square (Pos 'E' 8) (Just (Piece Black King)),
    Square (Pos 'F' 8) (Just (Piece Black Bishop)),
    Square (Pos 'G' 8) (Just (Piece Black Knight)),
    Square (Pos 'H' 8) (Just (Piece Black Rook)),
    Square (Pos 'A' 7) (Just (Piece Black Pawn)),
    Square (Pos 'B' 7) (Just (Piece Black Pawn)),
    Square (Pos 'C' 7) (Just (Piece Black Pawn)),
    Square (Pos 'D' 7) (Just (Piece Black Pawn)),
    Square (Pos 'E' 7) (Just (Piece Black Pawn)),
    Square (Pos 'F' 7) (Just (Piece Black Pawn)),
    Square (Pos 'G' 7) (Just (Piece Black Pawn)),
    Square (Pos 'H' 7) (Just (Piece Black Pawn))
  ]

-- prettyBoard :: Board -> String

-- Test with: 
--    evalBoard initialBoard
evalBoard :: Board -> Int
evalBoard (Board squares) = sum $ map valueSquare squares
    where
        valueSquare :: Square -> Int
        valueSquare (Square _ Nothing) = 0
        valueSquare (Square _ (Just piece)) = valuePiece piece

-- Can also try guards instead of case
valuePiece :: Piece -> Int
-- Test with: 
--    valuePiece (Piece White Pawn)
--    valuePiece (Piece Black King)
valuePiece (Piece White pieceType) =
    case pieceType of
    Pawn -> 1
    Knight -> 3
    Bishop -> 3
    Rook -> 5
    Queen -> 9
    King -> 1000
valuePiece (Piece Black pieceType) =
    case pieceType of
    Pawn -> -1
    Knight -> -3
    Bishop -> -3
    Rook -> -5
    Queen -> -9
    King -> -1000

-- TASK 2 --

-- movePos :: Pos -> Pos -> Board -> Board