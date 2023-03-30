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

gameLoop :: [Player] -> Deck -> Deck -> IO ()
gameLoop [] _ _ = return ()
gameLoop plrs deck pile = gameLoop' (fromList plrs) deck pile (length plrs)

gameLoop' :: CList Player -> Deck -> Deck -> Int -> IO ()
gameLoop' players deck pile 0 = do
    -- New round
    putStrLn "New Round!"
    sleep 1
    let players' = fromList (map (`resetMoves` standardMoves) (toList players))
    -- checks if win con is achived
    case defaultWinCon (toList players') deck pile of
        Just plr -> do
            putStrLn ("Player: " ++ name plr ++ " won!")
            sleep 1
            return ()
        _ -> gameLoop' players' deck pile (size players')
-- Main game
gameLoop' players deck pile n = do
    putStrLn "\ESC[2J"
    case focus players of
        Just plr -> do
            putStrLn (name plr ++ "'s turn!")
            print (head pile) -- displayes the card too be placed
            c <- getLine -- Gets player actions
            if not (isValidMove c plr) || not (hasMove plr (getMoveFromString c))
                then
                    do
                        putStrLn ("Invalid move, expected " ++ show (unique (moves plr)))
                        sleep 1
                        gameLoop' players deck pile n
                else
                    case getMoveFromPlayer plr (getMoveFromString c) of
                        -- Play card
                        PlayCard b -> do
                            if null (hand plr)
                                then
                                    do
                                        putStrLn "No cards on hand"
                                        sleep 1
                                        gameLoop' players deck pile n
                                else  -- Code to play a card
                                    do
                                        putStrLn "Hand:"
                                        print (hand plr)
                                        putStrLn ("Choose a card: (1/" ++ show (length (hand plr)) ++ ")")
                                        i <- getLine
                                        let cardIndex = read i :: Int
                                        let validIndexes = [1..(length (hand plr))]
                                        if cardIndex `elem` validIndexes  -- Checks if the given card index is valid
                                            then
                                                do
                                                    -- Gets card
                                                    let card = hand plr !! cardIndex
                                                    -- checks if card can be placed
                                                    if canPlaceCard card (head pile) sortingRules
                                                        then
                                                            do
                                                                putStrLn ("Plays " ++ show card ++ " on " ++ show (head pile))
                                                                let plr' = Player (name plr) (removeFirst (hand plr) card) (removeFirst (moves plr) (PlayCard b))
                                                                let pile' = card:pile
                                                                -- If player can play card again, 
                                                                if b
                                                                    then
                                                                        do
                                                                            sleep 2
                                                                            gameLoop' (update plr' players) deck pile' n
                                                                    else
                                                                        do
                                                                            sleep 2
                                                                            gameLoop' (rotR (update plr' players)) deck pile' (n - 1)
                                                        else
                                                            do
                                                                putStrLn "Cannot place that card"
                                                                sleep 1
                                                                gameLoop' players deck pile n


                                            else
                                                do
                                                    putStrLn ("Invalid move, expected a number between 1 and " ++ show (length (hand plr)))
                                                    sleep 1
                                                    gameLoop' players deck pile n
                        -- Draw card
                        DrawCard b -> do
                            if null deck
                                then
                                    do
                                        putStrLn "Deck is empty"
                                        sleep 1
                                        gameLoop' players deck pile n
                                else
                                    do
                                        let card = head deck
                                        putStrLn ("Drew " ++ show card)
                                        sleep 1
                                        let plr' = Player (name plr) (card:hand plr) (removeFirst (moves plr) (DrawCard b))
                                        let deck' = drop 1 deck
                                        if b
                                            then
                                                do
                                                    sleep 1
                                                    gameLoop' (update plr' players) deck' pile n
                                            else
                                                do
                                                    sleep 1
                                                    gameLoop' (rotR (update plr' players)) deck' pile (n - 1)
                        -- Passes round
                        Pass b -> do
                            if b
                                then
                                    do
                                        sleep 1
                                        let plr' = Player (name plr) (hand plr) (removeFirst (moves plr) (Pass b))
                                        gameLoop' (update plr' players) deck pile n
                                else
                                    do
                                        sleep 1
                                        let plr' = Player (name plr) (hand plr) (removeFirst (moves plr) (Pass b))
                                        gameLoop' (rotR (update plr' players)) deck pile (n - 1)
        _ -> if isEmpty players
                then
                    error "No players"
                else
                    gameLoop' (rotR players) deck pile n

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