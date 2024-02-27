module Chess where


-- TASK 1 --
import Data.Maybe (catMaybes, listToMaybe, isNothing)
import Data.List (intersperse)

-- Data types
data Piece = Piece PieceColor PieceType deriving (Show, Eq)
data PieceColor = White | Black | Null deriving (Show, Eq)
data PieceType = Dummy | Pawn | Bishop | Knight | Rook | Queen | King deriving (Show, Eq)

data Square = Square Pos (Maybe Piece) deriving (Show, Eq)
data Board = Board [Square] deriving (Show, Eq)
data Pos = Pos Char Int deriving (Show, Eq)
data State = State { gameState :: Board } deriving (Show, Eq)


initialBoard :: Board
initialBoard = Board $ pieceSquares ++ dummySquares
    where 
        pieceSquares = [
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
        dummySquares = [Square (Pos file rank) (Just (Piece Null Dummy)) | file <- ['A'..'H'], rank <- [1..8]]


prettySquare :: Maybe Piece -> Char
prettySquare Nothing = '~'
prettySquare (Just (Piece Null Dummy)) = '*'
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
--      prettyBoard initialBoard
prettyBoard :: Board -> IO()
prettyBoard (Board squares) =
    putStrLn $ unlines [intersperse ' ' [prettySquare (flatten (lookup (Pos file rank) pieceList)) | file <- ['A'..'H']] | rank <- [8,7..1]]
    where
        pieceList = [(p, s) | (Square p s) <- squares]

        flatten :: Maybe (Maybe a) -> Maybe a
        flatten Nothing = Nothing
        flatten (Just Nothing) = Nothing
        flatten (Just (Just a)) = Just a

-- Test with: 
--      evalBoard initialBoard
evalBoard :: Board -> Int
evalBoard (Board board) = sum $ map valueSquare board
    where
        valueSquare :: Square -> Int
        valueSquare (Square _ Nothing) = 0
        valueSquare (Square _ (Just piece)) = valuePiece piece

-- Test with: 
--      valuePiece (Piece White Pawn)
--      valuePiece (Piece Black King)
valuePiece :: Piece -> Int
valuePiece (Piece White pieceType) =
    case pieceType of
        Dummy -> 0
        Pawn -> 1
        Knight -> 3
        Bishop -> 3
        Rook -> 5
        Queen -> 9
        King -> 1000
valuePiece (Piece Black pType) =
    case pType of
        Dummy -> 0
        Pawn -> -1
        Knight -> -3
        Bishop -> -3
        Rook -> -5
        Queen -> -9
        King -> -1000

-- TASK 2 --

-- Test with:
--      prettyBoard (movePos (Pos 'A' 1) (Pos 'A' 8) initialBoard)
--      prettyBoard (movePos (Pos 'A' 1) (Pos 'B' 4) initialBoard)
movePos :: Pos -> Pos -> Board -> Board
movePos fromPos toPos board = deleteSquare fromPos (updateBoard fromPos toPos board)

-- Test with:
--      prettyBoard (deleteSquare (Pos 'G' 7) initialBoard)
deleteSquare :: Pos -> Board -> Board
deleteSquare pos (Board squares) = Board (map replaceIfMatch squares)
    where
        replaceIfMatch square@(Square squarePos _) = 
            if squarePos == pos 
            then Square squarePos (Just (Piece Null Dummy)) -- Replace with a Dummy piece
            else square

-- Test with:
--      prettyBoard (updateBoard (Pos 'A' 1) (Pos 'E' 8) initialBoard)
--      prettyBoard (updateBoard (Pos 'A' 1) (Pos 'E' 5) initialBoard)
updateBoard :: Pos -> Pos -> Board -> Board
updateBoard fromPos toPos (Board squares) = Board (map updateSquare squares)
    where
        updateSquare (Square pos piece)
            | pos == toPos   = Square toPos (getPiece fromPos (Board squares))
--            | pos == fromPos = Square fromPos (getPiece toPos (Board squares))
            | otherwise      = Square pos piece

-- Test with:
--      getPiece (Pos 'A' 1) initialBoard
--      getPiece (Pos 'E' 5) initialBoard
getPiece :: Pos -> Board -> Maybe Piece
getPiece pos (Board board) = 
    foldr (\(Square sqPos maybePiece) acc -> if sqPos == pos then maybePiece else acc) Nothing board

-- Test with:
--      moves Bishop
--      moves Rook
moves :: PieceType -> [(Int, Int)]
moves Dummy =   []
moves Pawn =    [] -- Pawn vectors depend on piece colour
moves Knight =  [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
moves Bishop =  [(1, 1), (1, -1), (-1, 1), (-1, -1)] -- Diagonals
moves Rook =    [(0, 1), (1, 0), (-1, 0), (0, -1)] -- Horizontals + verticals
moves King =    [(1, 1), (1, 0), (0, 1), (-1, -1), (-1, 0), (-1, 1), (0, -1), (1, -1)]

genMoves :: Board -> Pos -> [Board]
genMoves board pos = case getPiece pos board of
    Nothing -> []
    Just piece -> case pieceType piece of
        Pawn -> generatePawnMoves piece pos board
        _    -> concatMap (generatePieceMoves piece pos board) (moves (pieceType piece))

-- genMoves helper functions
pieceType :: Piece -> PieceType
pieceType (Piece _ piece) = piece

-- Test with:
--      prettyBoards (generatePawnMoves (Piece White Pawn) (Pos 'A' 2) initialBoard)
generatePawnMoves :: Piece -> Pos -> Board -> [Board]
generatePawnMoves (Piece color _) (Pos file rank) board =
    let stepDirection =   if color == White then 1 else -1
        startPos =        if color == White && rank == 2 || color == Black && rank == 7 then [1, 2] else [1]
        forwardMoves =    [movePos (Pos file rank) (Pos file (rank + n * stepDirection)) board | n <- startPos, isPosEmpty (Pos file (rank + n * stepDirection)) board]
        captureMoves =    [movePos (Pos file rank) (Pos (toEnum $ fromEnum file + d) (rank + stepDirection)) board |
                      d <- [-1, 1], isValidCapture (Pos (toEnum $ fromEnum file + d) (rank + stepDirection)) board color]
    in forwardMoves ++ captureMoves

-- Test with:
--      prettyBoards (generatePawnMoves (Piece White Knight) (Pos 'B' 1) initialBoard)
--      prettyBoards (generatePawnMoves (Piece White Pawn) (Pos 'A' 2) initialBoard)
generatePieceMoves :: Piece -> Pos -> Board -> (Int, Int) -> [Board]
generatePieceMoves piece pos board (file, rank) =
    let Pos deltaf deltar = pos
        newPos = Pos (toEnum $ fromEnum deltaf + file) (deltar + rank)
        Piece color _ = piece
    in if withinBoard newPos && (isPosEmpty newPos board || isValidCapture newPos board color)
        then [movePos pos newPos board]
        else []


-- Test with:
--      withinBoard (Pos 'A' 1)
--      withinBoard (Pos 'Z' 8)
--      withinBoard (Pos 'A' 20)
withinBoard :: Pos -> Bool
withinBoard (Pos file rank) = file >= 'A' && file <= 'H' && rank >= 1 && rank <= 8

-- Test with:
--      isPosEmpty (Pos 'E' 4) initialBoard
--      isPosEmpty (Pos 'F' 8) initialBoard
isPosEmpty :: Pos -> Board -> Bool
isPosEmpty pos board = case getPiece pos board of
    Just (Piece _ Dummy) -> True
    Nothing -> True  -- Should not reach here, but just in case
    _ -> False


-- Test with:
--      isValidCapture (Pos 'E' 7) initialBoard White
--      Returns true if input colour is opposite of the occupant's colour
isValidCapture :: Pos -> Board -> PieceColor -> Bool
isValidCapture newPos board color = case getPiece newPos board of
                                      Just (Piece c _) -> c /= color
                                      _ -> False

-- Prints several boards out from a list, useful for functions like genMoves
prettyBoards :: [Board] -> IO ()
prettyBoards = mapM_ prettyBoard

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
nextStates (State gamestate) = map State (concatMap (genMoves gamestate) allPos)
    where
        allPos = colorPos White gamestate ++ colorPos Black gamestate

-- Test with:
--      isGameOver (State initialBoard)
--      isGameOver (State (movePos (Pos 'A' 1) (Pos 'E' 8) initialBoard))
isGameOver :: State -> Bool
isGameOver (State board) = not $ (isBlackKingAlive board) && (isWhiteKingAlive board)
    where
        isBlackKingAlive :: Board -> Bool
        isBlackKingAlive (Board squares) = not $ [] == (filter (\(Square _ mbp) -> mbp == (Just (Piece Black King))) squares)

        isWhiteKingAlive :: Board -> Bool
        isWhiteKingAlive (Board squares) = not $ [] == (filter (\(Square _ mbp) -> mbp == (Just (Piece White King))) squares)
