{-# OPTIONS_GHC -Wno-missing-fields #-}
module PlayGame where

import System.Console.ANSI (clearScreen)
import Data.CircularList
    ( focus, fromList, rotR, update )
import System.Time.Extra ( sleep )

import Player ( createPlayers, Player (name, moves, hand), Move (PlayCard, DrawCard, Pass), isValidMove, getMoveFromString )
import Card
import Text.Read (readMaybe)
import ParseExpr (loadGame)
import Game (Game (players, pile, state, Game, gameName, deck, endCon, winCon), GameState (Start, TurnEnd, TurnStart), removeFirst, sortingRules, lookupOrDefault, unique, playerTurnStart)

gameLoop :: Game -> IO Game
gameLoop g = do
    clearScreen
    if any ($ g) (endCon g)
        then
            return g
        else
            do
                putStrLn ("Playing: " ++ gameName g ++ "!")
                putStrLn "\n\n\n\n\n"
                case focus (players g) of
                    Just p -> do
                        putStrLn ("It's " ++ name p ++ "'s turn!")
                        sleep 1
                        doPlayerTurn g p
                    Nothing -> gameLoop (g { players = rotR (players g) })



-- Returns once a player turn is over
doPlayerTurn :: Game  -> Player -> IO Game
doPlayerTurn game p = do
    let game' = game { state = TurnStart }
    let playerInfo = playerTurnStart p
    let pileInfo = "Pile: " ++ show (head (pile game))
    let terminal = playerInfo ++ pileInfo
    putStrLn terminal
    action <- getLine
    if not (isValidMove action p)
        then
            do
                putStrLn ("Invalid move, expected " ++ show (map fst (unique (moves p))))
                sleep 1
                doPlayerTurn game p
        else
            case getMoveFromString action of
                Just move -> case move of
                    PlayCard -> doPlayerActionPlayCard game' p (lookupOrDefault move False (moves p))
                    DrawCard -> doPlayerActionDrawCard game' p (lookupOrDefault move False (moves p))
                    Pass -> doPlayerActionPass game' p (lookupOrDefault move False (moves p))
                Nothing -> do
                    putStrLn ("Invalid move, expected " ++ show (map fst (unique (moves p))))
                    sleep 1
                    doPlayerTurn game p




-- Play Card
doPlayerActionPlayCard :: Game -> Player -> Bool -> IO Game
doPlayerActionPlayCard game plr continue = do
    if null (hand plr)
        then
            do
                putStrLn "No cards on hand"
                sleep 1
                return game
        else  -- Code to play a card
            do
                putStrLn ("Choose a card: (1/" ++ show (length (hand plr)) ++ ")")
                i <- getLine
                case i of
                    "q" -> doPlayerTurn game plr
                    _ -> do
                            case readMaybe i :: Maybe Int of
                                Just cardIndex -> do
                                    if cardIndex `elem` [1..(length (hand plr))]
                                        then
                                            do
                                                let card = hand plr !! cardIndex
                                                -- checks if card can be placed
                                                if canPlaceCard card (head (pile game)) sortingRules
                                                    then
                                                        do
                                                            putStrLn ("Plays " ++ show card ++ " on " ++ show (head (pile game)))
                                                            let plr' = plr { hand = removeFirst (hand plr) card, moves = removeFirst (moves plr) (PlayCard, continue) }
                                                            let pile' = card:pile game
                                                            -- If player can play card again, 
                                                            if continue
                                                                then
                                                                    do
                                                                        sleep 2
                                                                        let game' = game { pile = pile', players = update plr' (players game)}
                                                                        doPlayerTurn game' plr'
                                                                else
                                                                    do
                                                                        sleep 2
                                                                        return game { state = TurnEnd, pile = pile', players = rotR (update plr' (players game))}
                                                    else
                                                        do
                                                            putStrLn "Cannot place that card"
                                                            sleep 1
                                                            doPlayerActionPlayCard game plr continue
                                        else
                                            do
                                                putStrLn ("Invalid input " ++ show i ++ ", expected " ++ show [1..(length (hand plr))])
                                                doPlayerActionPlayCard game plr continue
                                _ -> do
                                    putStrLn ("Invalid input " ++ show i ++ ", expected and integer in " ++ show [1..(length (hand plr))])
                                    doPlayerActionPlayCard game plr continue




-- Draw card
doPlayerActionDrawCard :: Game -> Player -> Bool -> IO Game
doPlayerActionDrawCard game plr continue = do
    if null (deck game)
        then
            do
                putStrLn "Deck is empty"
                sleep 1
                doPlayerTurn game plr
        else
            do
                let card = head (deck game)
                putStrLn ("Drew " ++ show card)
                sleep 1
                let plr' = plr { hand = card:hand plr, moves = removeFirst (moves plr) (DrawCard, continue)}
                let game' = game { deck = drop 1 (deck game), players = update plr' (players game) }
                sleep 1
                if continue
                    then
                        do
                            doPlayerTurn game' plr'
                    else
                        do
                            return (game {state = TurnEnd})

-- Pass action
doPlayerActionPass :: Game -> Player -> Bool -> IO Game
doPlayerActionPass game plr continue = do
    sleep 1
    let plr' = plr {moves = removeFirst (moves plr) (Pass, continue)}
    if continue
        then
            do
                doPlayerTurn (game { players = update plr' (players game)}) plr'
        else
            do
                return (game { players = rotR (update plr' (players game))})



gameStart :: String -> IO ()
gameStart gamename = do
    plrs <- createPlayer
    game <- loadGame gamename (Game { players = fromList plrs, state = Start })
    game' <- gameLoop game
    gameEnd game'

gameEnd :: Game -> IO ()
gameEnd g = do
    let winner = head (head (map ($ g) (winCon g)))
    putStrLn ("The winner is: " ++ name winner ++ "!")
    sleep 2

createPlayer :: IO [Player]
createPlayer = do
    putStrLn "How many players?"
    c <- getLine
    case readMaybe c :: Maybe Int of
        Just i -> createPlayers i
        Nothing -> do
            putStrLn "Invalid input, please enter a number greater than 2"
            sleep 1
            createPlayer

