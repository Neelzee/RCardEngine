module Main where

import Data.List (sortOn)
import System.Directory (listDirectory, renameFile, doesDirectoryExist, createDirectoryIfMissing)
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
import Feature
import Functions (allGames, lookupM)
import Terminal.ExecGameCommands
    ( execGameCommands, confirmCommand, printGCEffect, fromCDSLParseErrorOnLoad,  )
import Terminal.GameCommands (GCError (GCError, errType, UnknownFlagsError, input, UnknownCommandError, InvalidCommandArgumentError, CDSLError, MissingOrCorruptDataError), GameCommand (Help, Create, Save, cmd, Edit, Add, Update, Test, Remove, Copy, Rename, Status, Close, Clear, Quit, List, Play), Flag, commands, showAll)
import Constants (gameFolder)
import qualified Data.Map as Map

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
                                let im = (Map.fromList [((GameName, Just []), [Text gm])])
                                editor (Map.fromList [(GameAttributes, im)])
                                mainLoop
                        else
                            mainLoop
            -- Edit Command
            (Edit (Numeric i) flg) -> do
                g <- allGames
                if i `elem` [0..(length g)]
                    then
                        do
                            res <- loadGameData Map.empty i
                            case res of
                                Left gd' -> do
                                    gce <- case Map.lookup GameAttributes gd' of
                                        Just att -> case lookupM GameName att of
                                            Just (_, [Text gm]) -> return [GCEffect { se = "Loaded GameData: " ++ gm
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
                                    printGCEffect [(fromCDSLParseErrorOnLoad err)] flg
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
                unless res mainLoop
            -- Generic commands
            _ -> do
                execGameCommands gc
                mainLoop

-- Main program entry point
main :: IO ()
main = do
    setup
    clearScreen
    putStrLn "Welcome to CardEngine"
    mainLoop



setup :: IO ()
setup = do
    let dir = gameFolder
    exists <- doesDirectoryExist dir
    createDirectoryIfMissing exists dir