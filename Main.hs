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
    let initialState = State initialBoard White -- Assuming White starts the game

    playLoop initialState
    where
        playLoop :: State -> IO ()
        playLoop state = do
            if isGameOver state
                then putStrLn "Game over."
                else do
                    prettyBoard (gameState state) -- Assuming a function to print the board
                    putStrLn $ show (currentPlayer state) ++ "'s turn."
                    -- Get move from player and make the move
                    newState <- playerTurn (gameState state) (currentPlayer state)
                    let nextPlayer = if currentPlayer state == White then Black else White
                    playLoop $ State newState nextPlayer


main :: IO ()
main = do
    playGame
