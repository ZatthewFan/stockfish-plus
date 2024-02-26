-- TASK 1 --
import Data.Maybe (catMaybes)
import Data.List (intersperse)

-- Data types
data Piece = Piece PieceColor PieceType deriving (Show, Eq)
data PieceColor = White | Black deriving (Show, Eq)
data PieceType = Pawn | Bishop | Knight | Rook | Queen | King deriving (Show, Eq)

data Square = Square Pos (Maybe Piece) deriving (Show, Eq)
data Board = Board [Square] deriving (Show, Eq)
data Pos = Pos Char Int deriving (Show, Eq)
data State = State { gameState :: Board } deriving (Show, Eq)


initialBoard :: Board
initialBoard = Board [
    Square (Pos 'A' 1) (Just (Piece White Rook)),
    Square (Pos 'B' 1) (Just (Piece White Knight)),
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
    Square (Pos 'A' 7) (Just (Piece Black Pawn)),
    Square (Pos 'B' 7) (Just (Piece Black Pawn)),
    Square (Pos 'C' 7) (Just (Piece Black Pawn)),
    Square (Pos 'D' 7) (Just (Piece Black Pawn)),
    Square (Pos 'E' 7) (Just (Piece Black Pawn)),
    Square (Pos 'F' 7) (Just (Piece Black Pawn)),
    Square (Pos 'G' 7) (Just (Piece Black Pawn)),
    Square (Pos 'H' 7) (Just (Piece Black Pawn)),
    Square (Pos 'A' 8) (Just (Piece Black Rook)),
    Square (Pos 'B' 8) (Just (Piece Black Knight)),
    Square (Pos 'C' 8) (Just (Piece Black Bishop)),
    Square (Pos 'D' 8) (Just (Piece Black Queen)),
    Square (Pos 'E' 8) (Just (Piece Black King)),
    Square (Pos 'F' 8) (Just (Piece Black Bishop)),
    Square (Pos 'G' 8) (Just (Piece Black Knight)),
    Square (Pos 'H' 8) (Just (Piece Black Rook))
    ]

prettySquare :: Maybe Piece -> Char
prettySquare Nothing = '*'
prettySquare (Just (Piece White Pawn)) = '♟'
prettySquare (Just (Piece Black Pawn)) = '♙'
prettySquare (Just (Piece White Rook)) = '♜'
prettySquare (Just (Piece Black Rook)) = '♖'
prettySquare (Just (Piece White Knight)) = '♞'
prettySquare (Just (Piece Black Knight)) = '♘'
prettySquare (Just (Piece White Bishop)) = '♝'
prettySquare (Just (Piece Black Bishop)) = '♗'
prettySquare (Just (Piece White Queen)) = '♛'
prettySquare (Just (Piece Black Queen)) = '♕'
prettySquare (Just (Piece White King)) = '♚'
prettySquare (Just (Piece Black King)) = '♔'

-- Test with:
--      putStrLn $ prettyBoard initialBoard
prettyBoard :: Board -> String
prettyBoard (Board squares) =
    unlines [intersperse ' ' [prettySquare (flatten (lookup (Pos file rank) pieceList)) | file <- ['A'..'H']] | rank <- [1..8]]
    where
        pieceList = [(p, s) | (Square p s) <- squares]

        -- Test with:
        --      flatten Nothing
        --      flatten (Just Nothing)
        --      flatten (Just (Just (Piece Black King)))
        flatten :: Maybe (Maybe a) -> Maybe a
        flatten Nothing = Nothing
        flatten (Just Nothing) = Nothing
        flatten (Just (Just a)) = Just a

-- Test with: 
--      evalBoard initialBoard
evalBoard :: Board -> Int
evalBoard (Board squares) = sum $ map valueSquare squares
    where
        valueSquare :: Square -> Int
        valueSquare (Square _ Nothing) = 0
        valueSquare (Square _ (Just piece)) = valuePiece piece

-- Can also try guards instead of case
-- Test with: 
--      valuePiece (Piece White Pawn)
--      valuePiece (Piece Black King)
valuePiece :: Piece -> Int
valuePiece (Piece White pieceType) =
    case pieceType of
    Pawn -> 1
    Knight -> 3
    Bishop -> 3
    Rook -> 5
    Queen -> 9
    King -> 1000
valuePiece (Piece Black pType) =
    case pType of
    Pawn -> -1
    Knight -> -3
    Bishop -> -3
    Rook -> -5
    Queen -> -9
    King -> -1000

-- TASK 2 --

movePos :: Pos -> Pos -> Board -> Board
movePos fromPos toPos board = deleteSquare toPos (updateBoard fromPos toPos board)
-- Test with:
--      prettyBoard (deleteSquare (Pos 'G' 7) initialBoard)
deleteSquare :: Pos -> Board -> Board
deleteSquare pos (Board board) = Board (filter (\(Square squarePos _) -> squarePos /= pos) board)

-- Test with:
--      prettyBoard (updateBoard (Pos 'A' 1) (Pos 'A' 8 ) initialBoard)
updateBoard :: Pos -> Pos -> Board -> Board
updateBoard fromPos toPos (Board board) = Board (map (updateSquare toPos fromPos (Board board)) board)

-- Test with:
--      updateSquare (Pos 'A' 1) (Pos 'A' 8) initialBoard (Square (Pos 'A' 8) (Just (Piece White Rook)))
--      updateSquare (Pos 'A' 1) (Pos 'A' 8) initialBoard (Square (Pos 'A' 1) (Just (Piece White Rook)))
updateSquare :: Pos -> Pos -> Board -> Square -> Square
updateSquare fromPos toPos board (Square pos piece)
    | pos == toPos =    Square toPos (getPiece fromPos board)
    | otherwise =       Square pos piece

-- Test with:
--      getPiece (Pos 'A' 1) initialBoard
getPiece :: Pos -> Board -> Maybe Piece
getPiece p (Board board) = piece
    where
        (Square _ piece) = head [sq | sq@(Square pos _) <- board, pos == p]

-- Test with:
--      moves Bishop
--      moves Rook
moves :: PieceType -> [(Int, Int)]
moves Pawn =    [] -- Pawn vectors depend on piece colour
moves Knight = [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
moves Bishop = [(1, 1), (1, -1), (-1, 1), (-1, -1)] -- Diagonals
moves Rook = [(0, 1), (1, 0), (-1, 0), (0, -1)] -- Horizontals + verticals
moves King = [(1, 1), (1, 0), (0, 1), (-1, -1), (-1, 0), (-1, 1), (0, -1), (1, -1)]

-- Test with:
--      colorPos White initialBoard
colorPos :: PieceColor -> Board -> [Pos]
colorPos pieceColor (Board squares) = map snd $ filter (\(color, pos) -> color == pieceColor) [(getColorFromMaybePiece mbp, pos) | Square pos mbp <- squares]
    where
        getColorFromMaybePiece :: Maybe Piece -> PieceColor
        getColorFromMaybePiece Nothing = error "No piece provided"
        getColorFromMaybePiece (Just (Piece pc pt)) = pc

-- Test with:
--      nextStates (State initialBoard)
nextStates :: State -> [State]
nextStates (State gamestate) = concatMap (genMoves gamestate) allPos
    where
        allPos = colorPos White gamestate ++ colorPos Black gamestate
