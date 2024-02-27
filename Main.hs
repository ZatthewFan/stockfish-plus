module Main where

import Chess

parsePos :: String -> Pos
parsePos (x:xs) = Pos x (read xs)

playerTurn :: Board -> PieceColor -> IO Board
playerTurn board color = do
    -- Print board
    prettyBoard board
    putStrLn $ show color ++ "'s turn."
    
    -- Prompt for player input
    putStrLn "Enter your move in the format 'fromPos toPos' (example: 'A2 A3')."
    input <- getLine
    let [from, to] = words input
    let fromPos = parsePos from
    let toPos   = parsePos to

    -- Check move validity??
    -- 
    --
    

    return $ movePos fromPos toPos board


playGame :: IO ()
playGame = do
    let initialState = (State initialBoard)
    playLoop initialState
    where
        playLoop :: State -> IO ()
        playLoop state = do
            if isGameOver state
                then putStrLn "Game over."
                else do
                    let currentPlayerColor = if length (colorPos White (gameState state)) > length (colorPos Black (gameState state)) then Black else White
                    newState <- playerTurn (gameState state) currentPlayerColor
                    playLoop (State newState)

main :: IO ()
main = do
    playGame
