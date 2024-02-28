module Main where

import Chess

parsePos :: String -> Pos
parsePos (x:xs) = Pos x (read xs)

validPos :: Pos -> Bool
validPos (Pos file rank) = file `elem` ['A'..'H'] && rank `elem` [1..8]

playerTurn :: Board -> PieceColor -> IO Board
playerTurn board color = do
    -- Prompt for player input
    putStrLn "Enter your move in the format 'fromPos toPos' (example: 'A2 A3'). \nEnter Ctrl-c to exit game"
    input <- getLine
    if length (words input) /= 2
        then do
            putStrLn "Invalid move format. Try again using the format provided by the example"
            playerTurn board color
    else do
        let [from, to] = words input
        let fromPos = parsePos from
        let toPos   = parsePos to
        
        -- check move validity
        if not $ validPos fromPos && validPos toPos
            then do
                putStrLn "Invalid move. Try again"
                playerTurn board color
            else return $ movePos fromPos toPos board

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
