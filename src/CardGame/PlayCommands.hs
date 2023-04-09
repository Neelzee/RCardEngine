{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module CardGame.PlayCommands where

import Terminal.GameCommands (GCError (GCError, InvalidCommandArgumentError, errType, input, UnknownCommandError), GCEffect (gcErr, se, ve))
import Data.List.Extra (trim)
import Text.Read (readMaybe)
import Terminal.ExecGameCommands (printTable)


data PLCommand =
    PLCommand
    {
        plc :: UserActions
    }
    deriving (Show, Eq)

data UserActions =
    Play Int
    | Draw Int
    | PassTurn
    | Moves
    | HandUA
    | ScoreUA
    | QuitUA
    | HelpUA
    deriving (Eq)

instance Show UserActions where
    show x = case x of
        (Play _) -> "play"
        (Draw _) -> "draw"
        PassTurn -> "pass"
        Moves -> "moves"
        HandUA -> "hand"
        ScoreUA -> "score"
        QuitUA -> "quit"
        HelpUA -> "help"


validatePLCommand :: String -> Either PLCommand GCError
validatePLCommand xs = case map trim (words xs) of
    ("play":ys) -> case readMaybe (concat ys) :: Maybe Int of
        Just i -> Left (PLCommand (Play i))
        Nothing -> Right (GCError { 
            errType = InvalidCommandArgumentError xs
            , input = xs})
    ("draw":ys) -> case readMaybe (concat ys) :: Maybe Int of
        Just i -> Left (PLCommand (Draw i))
        Nothing -> Right (GCError { 
            errType = InvalidCommandArgumentError xs
            , input = xs})
    ["pass"] -> Left (PLCommand PassTurn)
    ["hand"] -> Left (PLCommand HandUA)
    ["Score"] -> Left (PLCommand ScoreUA)
    ["quit"] -> Left (PLCommand QuitUA)
    ["moves"] -> Left (PLCommand Moves)
    ["help"] -> Left (PLCommand HelpUA)


    _ -> Right (GCError { 
            errType = UnknownCommandError xs
            , input = xs})


plCommands :: [PLCommand]
plCommands = [
    PLCommand (Play 0)
    , PLCommand (Draw 0)
    , PLCommand PassTurn
    , PLCommand Moves
    , PLCommand HandUA
    , PLCommand ScoreUA
    , PLCommand QuitUA
    , PLCommand HelpUA
    ]


printUACommands :: IO ()
printUACommands = printTable (pc plCommands)
    where
        pc [] = []
        pc ((PLCommand c):xs) = (show c, info c, example c) : pc xs


info :: UserActions -> String
info x = case x of
    (Play _) -> "Plays the given card from your hand, card indexing starts at 1."
    (Draw _) -> "Draws the specified amount of cards, if you can draw cards."
    PassTurn -> "Passes the turn onto the next Player."
    Moves -> "List the moves you can do this turn."
    HandUA -> "Shows your hand."
    ScoreUA -> "Shows your score."
    QuitUA -> "Quits the game for you."
    HelpUA -> "Shows this screen."

example :: UserActions -> String
example x = case x of
    (Play _) -> "play 1"
    (Draw _) -> "draw 2"
    _ -> show x