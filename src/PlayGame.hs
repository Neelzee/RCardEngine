{-# OPTIONS_GHC -Wno-missing-fields #-}
module PlayGame where

import Data.CircularList
    ( focus, fromList, rotR, update, isEmpty )
import System.Time.Extra ( sleep )

import Player ( createPlayers, Player (name, moves, hand), Move (PlayCard, DrawCard, Pass), isValidMove, getMoveFromString, resetMoves, standardMoves )
import Card
import Text.Read (readMaybe)
import ParseExpr (loadGame, parsePlayerMoves, lookupAll)
import Game (Game (players, pile, state, Game, gameName, deck, endCon, winCon, rules, actions, canPlaceCard), GameState (Start, TurnEnd, TurnStart), removeFirst, sortingRules, lookupOrDefault, unique, playerTurnStart, dealCards, gameActions)
import GameRules (GameRule(PlayerHand, PileCount, PlayerMoves))
import Data.Maybe (fromMaybe)
import System.Console.ANSI (clearScreen)
import Data.List (find)

gameLoop :: Game -> IO Game
gameLoop g = do
    --clearScreen
    if any ($ g) (endCon g)
        then
            do
                putStrLn "game over"
                return g
        else
            do
                putStrLn ("Playing: " ++ gameName g ++ "!")
                putStrLn "\n\n\n\n\n"
                case focus (players g) of
                    Just p -> do
                        putStrLn ("It's " ++ name p ++ "'s turn!")
                        sleep 1
                        g' <- doPlayerTurn g p
                        gameLoop g'
                    Nothing -> if isEmpty (players g)
                        then
                            error "no players"
                        else
                            gameLoop g { players = rotR (players g)}



-- Returns once a player turn is over
doPlayerTurn :: Game  -> Player -> IO Game
doPlayerTurn game plr = do
    let g' = gameActions (lookupAll (state game) (actions game)) game
    p <- case lookup PlayerMoves (rules game) of
        Just mv -> do
            return (resetMoves plr (parsePlayerMoves mv))
        Nothing -> return (resetMoves plr standardMoves)
    let game' = g' { state = TurnStart }
    -- Turn actions
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
                    PlayCard ->
                        case lookup move (moves p) of
                            Just True -> do
                                g <- doPlayerActionPlayCard game' p True
                                doPlayerTurn g (fromMaybe p (focus (players g)))
                            _ -> do
                                doPlayerActionPlayCard game' p False
                    DrawCard ->
                        case lookup move (moves p) of
                            Just True -> do
                                g <- doPlayerActionDrawCard game' p True
                                doPlayerTurn g (fromMaybe p (focus (players g)))
                            _ -> do
                                doPlayerActionDrawCard game' p False
                    Pass ->
                        case lookup move (moves p) of
                            Just True -> do
                                g <- doPlayerActionPass game' p (lookupOrDefault move False (moves p))
                                doPlayerTurn g (fromMaybe p (focus (players g)))
                            _ -> do
                                doPlayerActionPass game' p False
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
                putStrLn ("Choose a card: (1/" ++ show (length (hand plr)) ++ ") or (q to cancel)")
                i <- getLine
                case i of
                    "q" -> doPlayerTurn game plr
                    _ -> do
                            case readMaybe i :: Maybe Int of
                                Just cardIndex -> do
                                    if cardIndex `elem` [1..(length (hand plr))]
                                        then
                                            do
                                                let card = hand plr !! (cardIndex - 1)
                                                -- checks if card can be placed
                                                if foldr ((&&) . (\f -> uncurry f (game, card))) True (canPlaceCard game)
                                                    then
                                                        do
                                                            putStrLn ("Plays " ++ show card ++ " on " ++ show (head (pile game)))
                                                            let plr' = plr { hand = removeFirst (hand plr) card, moves = removeFirst (moves plr) (PlayCard, continue) }
                                                            -- If player can play card again, 
                                                            if continue
                                                                then
                                                                    do
                                                                        sleep 2
                                                                        let game' = game { pile = card:pile game, players = update plr' (players game)}
                                                                        doPlayerTurn game' plr'
                                                                else
                                                                    do
                                                                        sleep 2
                                                                        let g' = game { state = TurnEnd, pile = card:pile game, players = rotR (update plr' (players game))}
                                                                        return g'
                                                    else
                                                        do
                                                            putStrLn ("Cannot place " ++ show card ++ " on " ++ show (head (pile game)))
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
    game' <- case lookup PlayerHand (rules game) of
        Just n -> case readMaybe n :: Maybe Int of
            Just c -> return (dealCards game c)
            Nothing -> return (dealCards game 3)
        Nothing -> return (dealCards game 3)
    let game'' = game' {
        pile = case lookup PileCount (rules game') of
            Just pc -> case readMaybe pc :: Maybe Int of
                Just i -> take i (deck game)
                Nothing -> take 1 (deck game)
            Nothing -> take 1 (deck game)
        , deck = case lookup PileCount (rules game') of
            Just pc -> case readMaybe pc :: Maybe Int of
                Just i -> drop i (deck game)
                Nothing -> drop 1 (deck game)
            Nothing -> drop 1 (deck game)
    }

    let g = gameActions (lookupAll Start (actions game'')) game''

    game''' <- gameLoop g
    gameEnd game'''

gameEnd :: Game -> IO ()
gameEnd g = case find (not . null) (map ($ g) (winCon g)) of
    Just (x:_) ->  putStrLn ("The winner is: " ++ name x ++ "!")
    _ -> putStrLn "No winners!"

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

