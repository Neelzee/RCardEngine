{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module CardGame.PlayGame where

import Data.CircularList
    ( focus, fromList, rotR, update, toList, removeR )
import System.Time.Extra ( sleep )

import CardGame.Player ( createPlayers, Player (name, moves, hand, pScore), prettyPrintMoves )
import Text.Read (readMaybe)
import Data.List (find, intercalate)
import Feature ( Feature(PileCount, PlayerHand) )
import CDSL.CDSLExpr (CDSLExpr(Numeric))
import LoadGame (loadGame)
import Functions (lookupAll, removeFirst, count, dropFilteredCount, unique)
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass))
import System.IO ( hFlush, stdout )
import CardGame.PlayCommands (validatePLCommand, PLCommand (plc), UserActions (Play, Draw, PassTurn, HelpUA, Moves, HandUA, ScoreUA, QuitUA), printUACommands)
import System.Console.ANSI (clearScreen)
import CardGame.Game (Game (players, state, Game, pile, deck, actions, rules, winCon, canPlaceCard, gameName, endCon), GameState (Start, TurnEnd, TurnStart), dealCards, gameActions)
import CardGame.CardFunctions

gameLoop :: Game -> IO Game
gameLoop g = do
    clearScreen

    if null (players g)
        then
            do
                putStrLn "No players left."
                return g
        else
            if any ($ g) (endCon g)
                then
                    do
                        putStrLn "game over"
                        return g
                else
                    do
                        g' <- doPlayerTurn g
                        -- new player turn
                        gameLoop (g' { players = rotR (players g') })



-- Returns once a player turn is over
doPlayerTurn :: Game -> IO Game
doPlayerTurn g = do
    -- Applies actions on game start
    let g' = g { state = TurnStart }
    let game = gameActions (lookupAll (state g') (actions g')) g'
    case focus (players game) of
        Just plr -> do
            putStr ("Playing -> " ++ gameName game ++ " -> " ++ name plr ++ " > ")
            hFlush stdout
            putStr "\nPile: "
            case pile game of
                [] -> putStrLn "no cards on deck"
                p -> prettyPrintCards [head p]
            action <- getLine
            case validatePLCommand action of
                Left cm -> case plc cm of
                    (Play cardIndex) -> case lookup PlayCard (moves plr) of
                        Just b -> if cardIndex `elem` [1..(length (hand plr))]
                            then
                                do
                                    let card = hand plr !! (cardIndex - 1)
                                    -- checks if card can be placed
                                    if foldr ((&&) . (\f -> f game card)) True (canPlaceCard game)
                                        then
                                            do
                                                putStrLn ("Plays " ++ show card ++ " on " ++ show (head (pile game)))
                                                let plr' = plr { hand = removeFirst (hand plr) card, moves = removeFirst (moves plr) (PlayCard, b) }
                                                -- If player can play card again, 
                                                if b
                                                    then
                                                        doPlayerTurn game { pile = card:pile game, players = update plr' (players game)}
                                                    else
                                                        do
                                                            putStrLn "Hit Enter to go to next turn."
                                                            getLine
                                                            return game { state = TurnEnd, pile = card:pile game, players = update plr' (players game)}
                                        else
                                            do
                                                putStrLn ("Cannot place " ++ show card ++ " on " ++ show (head (pile game)))
                                                sleep 1
                                                doPlayerTurn game
                            else
                                do
                                    putStrLn ("Invalid input, expected number in the range " ++ show [1..(length (hand plr))])
                                    doPlayerTurn game
                        _ -> do
                            putStrLn "Cannot Play a card this turn, type 'moves', to show available moves."
                            doPlayerTurn game

                    (Draw c) -> do
                        let bls = lookupAll DrawCard (moves plr)
                        let tC = count True bls
                        let fC = length bls - tC
                        case (tC >= c, fC == 1 && (tC + 1) >= c) of
                            -- Can do action again
                            (True, _) -> do
                                let crds = take c (deck game)
                                putStrLn ("Drew: " ++ intercalate ", " (map show crds))
                                let p' = plr {hand = crds ++ hand plr, moves = dropFilteredCount (== (DrawCard, True)) c (moves plr)}
                                doPlayerTurn (game { deck = drop c (deck game), players = update p' (players game) })
                            -- Cannot do action again
                            (False, True) -> do
                                let crds = take c (deck game)
                                putStrLn ("Drew: " ++ intercalate ", " (map show crds))
                                let p' = plr {hand = crds ++ hand plr, moves = filter (\(m, _) -> m == DrawCard) (moves plr) }
                                putStrLn "Hit Enter to go to next turn."
                                getLine
                                return (game { deck = drop c (deck game), players = update p' (players game) })
                            -- Invalid
                            _ -> do
                                putStrLn "Cannot Draw a card this turn, type 'moves', to show available moves."
                                doPlayerTurn game

                    PassTurn -> case lookup Pass (moves plr) of
                        Just b ->
                            do
                                let p' = plr { moves = removeFirst (moves plr) (Pass, b) }
                                if b
                                    then
                                        doPlayerTurn game { players = update p' (players game) }
                                    else
                                        do
                                            nm <- case focus (rotR (players game)) of
                                                Just p -> return ("Hit Enter to go to " ++ name p ++ "'s turn.")
                                                Nothing -> return "Hit Enter to go to next turn."
                                            putStrLn nm
                                            getLine
                                            return game { players = update p' (players game) }
                        _ -> do
                            putStrLn "Cannot Pass this turn, type 'moves', to show available moves."
                            doPlayerTurn game

                    Moves -> do
                        let mv = moves plr
                        putStr "\nYour moves: "
                        prettyPrintMoves (unique mv)
                        doPlayerTurn game

                    HandUA -> do
                        let cd = hand plr
                        putStr "\nYour hand: "
                        prettyPrintCards cd
                        doPlayerTurn game

                    ScoreUA -> do
                        putStrLn ("Your score: " ++ show (pScore plr))
                        doPlayerTurn game


                    QuitUA -> do
                        putStrLn "You wanna quit the game? (yes/n, default = n)"
                        ans <- getLine
                        case words ans of
                            ["yes"] -> return (game { players = removeR (players game)})
                            _ -> doPlayerTurn game


                    HelpUA -> do
                        printUACommands
                        doPlayerTurn game

                _ -> do
                    putStrLn "Unknown command, type 'help' to list all commands."
                    doPlayerTurn game
        _ -> return game




gameStart :: Int -> IO ()
gameStart gi = do
    plrs <- createPlayer
    game <- loadGame (Game { players = fromList plrs, state = Start }) gi
    game' <- case lookup PlayerHand (rules game) of
        Just [Numeric n] -> return (dealCards game n)
        _ -> return (dealCards game 3)
    

    let g = gameActions (lookupAll Start (actions game')) game'

    game'' <- gameLoop g
    gameEnd game''

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

