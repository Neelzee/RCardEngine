module Main where


import Data.CircularList
    ( focus, fromList, isEmpty, rotR, size, toList, update, CList )
import System.Time.Extra ( sleep )

import Player
import Card
import Game

main :: IO ()
main = do
    game <- createGame
    game' <- gameLoop game
    putStrLn "Main is over"

gameLoop :: Game -> IO Game
gameLoop game = do
    game' <- doPlayerTurn game
    if (endCon game') game'
        then
            do
                putStrLn "Game Over!"
                return game'
        else
            do
                -- Increments counter
                let nGame = incrementRoundCounter game'
                -- Resets player moves
                let plrs = map (\p -> resetMoves p standardMoves) (toList (players nGame))
                -- Adds them back to the game
                let pGame = updatePlayers nGame (fromList plrs)
                gameLoop pGame

-- Removes the element at the given index
removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (_:xs) 0 = xs
removeAt (x:xs) n = x : removeAt xs (n - 1)