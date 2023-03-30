module Main where


import Data.CircularList
    ( focus, fromList, isEmpty, rotR, size, toList, update, CList )
import System.Time.Extra ( sleep )

import Player
import Card
import Game

main :: IO ()
main = do
  let players = [Player "Nils" [] standardMoves, Player "Kaspar" [] standardMoves]
  let (players', remainingDeck) = deal 10 newDeck players
  let remainingDeck' = drop 1 remainingDeck
  gameLoop players' remainingDeck' [head remainingDeck]

gameLoop :: Game -> IO Game
gameLoop game = do
    game' <- doPlayerTurn game
    if endCon game'
        then
            putStrLn "Game Over!"
            return game'
        else
            gameLoop game'

standardMoves :: Moves
standardMoves = [
    PlayCard False,
    DrawCard True,
    DrawCard True,
    DrawCard True,
    Pass False]


-- Removes the element at the given index
removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (_:xs) 0 = xs
removeAt (x:xs) n = x : removeAt xs (n - 1)

-- Removes the first instance of the given element
removeFirst :: Eq a => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) y = if x == y
    then
        xs
    else
        x : removeFirst xs y

-- Removes all duplicate elements from the given list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if x `elem` xs
    then
        unique xs
    else
        x : unique xs

-- Checks if the typed action is an action the player can do
isValidMove :: String -> Player -> Bool
isValidMove c (Player _ _ (x:xs)) = True


sortingRules :: Deck
sortingRules = reverse newDeck

defaultWinCon :: [Player] -> Deck -> Deck -> Maybe Player
defaultWinCon [] _ _ = Nothing
defaultWinCon (plr@(Player _ [] _):_) _ _ = Just plr
defaultWinCon (_:ps) dck pls = defaultWinCon ps dck pls