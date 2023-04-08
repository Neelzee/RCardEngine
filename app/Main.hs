module Main where

import Data.List (sortOn)
import System.Directory (listDirectory, renameFile)
import System.Console.ANSI (clearScreen)
import Control.Monad (unless, when)
import System.IO ( hFlush, stdout )
import CardGame.PlayGame (gameStart)
import Text.Read (readMaybe)
import GameData.GD (GameData)
import GameData.LoadGD (loadGameData)
import CDSL.CDSLExpr ( CDSLExpr(Numeric, Text) )
import Terminal.GameCommands (GameCommand(Play, Create), GCEffect (GCEffect, se, ve, gcErr))
import Terminal.ValidateGameCommands (validateGameCommand)
import GameEditor (editor)
import Feature (Feature(GameName))
import Functions (allGames)
import Terminal.ExecGameCommands
    ( execGameCommands, confirmCommand, printGCEffect, fromCDSLParseErrorOnLoad,  )
import Terminal.GameCommands (GCError (GCError, errType, UnknownFlagsError, input, UnknownCommandError, InvalidCommandArgumentError, CDSLError, MissingOrCorruptDataError), GameCommand (Help, Create, Save, cmd, Edit, Add, Update, Test, Remove, Copy, Rename, Status, Close, Clear, Quit, List, Play), Flag, commands, showAll)

-- Play the selected game
playGame :: Int -> IO ()
playGame n = do
    games <- listDirectory "games"
    if n >= 0 && n < length games
        then
            do
            gameStart n
        else
            putStrLn "Invalid game number."


-- Main program loop
mainLoop :: IO ()
mainLoop = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case validateGameCommand input of
        Right err -> do
            print err
            mainLoop
        Left gc -> case gc of
            (Play (Numeric i)) -> do
                playGame i
                mainLoop
            -- Create Command
            (Create (Text gm) flg) -> do
                    let gce = [GCEffect { se = "Created GameData: " ++ gm, ve = "Created GameData: " ++ gm, gcErr = [] }]
                    res <- confirmCommand gc gce flg
                    if res
                        then
                            do
                                editor [(GameName, [Text gm])]
                                mainLoop
                        else
                            mainLoop
            -- Edit Command
            (Edit (Numeric i) flg) -> do
                g <- allGames
                if i `elem` [0..(length g)]
                    then
                        do
                            res <- loadGameData [] i
                            case res of
                                Left gd' -> do
                                    gce <- case lookup GameName gd' of
                                        Just [Text gm] -> return [GCEffect { se = "Loaded GameData: " ++ gm
                                            , ve = "Loaded GameData: " ++ gm
                                            , gcErr = [] }]
                                        _ -> return [GCEffect { se = "Loaded GameData"
                                            , ve = "Loaded GameData"
                                            , gcErr = [GCError { errType = MissingOrCorruptDataError "No GameName Found", input = showAll gc}] }]
                                    r <- confirmCommand gc gce flg
                                    if r
                                        then
                                            do
                                                editor gd'
                                                mainLoop
                                        else
                                            mainLoop
                                Right err -> do
                                    printGCEffect (map fromCDSLParseErrorOnLoad err) flg
                                    mainLoop
                    else
                        do
                            printGCEffect [GCEffect { se = "Invalid number: '" ++ show i ++ "'"
                                , ve = "Invalid number: '" ++ show i ++ "', expected a number in the range of 0 to " ++ show (length g)
                                , gcErr = [GCError { errType = InvalidCommandArgumentError (show i), input = showAll gc }] }] flg
                            return ()
            -- Quit
            (Quit flgs) -> do
                res <- confirmCommand gc [GCEffect { se = "Quitting program.", ve = "Quitting program.", gcErr = [] }] flgs
                if res
                    then
                        return ()
                    else
                        mainLoop
            -- Generic commands
            _ -> do
                execGameCommands gc
                mainLoop

-- Main program entry point
main :: IO ()
main = do
    clearScreen
    putStrLn "Welcome to CardEngine"
    mainLoop

