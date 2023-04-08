module Main where

import Data.List (sortOn)
import System.Directory (listDirectory, renameFile)
import System.Console.ANSI (clearScreen)
import Control.Monad (unless, when)
import System.IO ( hFlush, stdout )
import CardGame.PlayGame (gameStart)
import Text.Read (readMaybe)
import GameData.GD (GameData)
import CDSL.CDSLExpr ( CDSLExpr(Numeric, Text) )
import Terminal.GameCommands (GameCommand(Play, Create), GCEffect (GCEffect, se, ve, gcErr))
import Terminal.ValidateGameCommands (validateGameCommand)
import GameEditor (editor)
import Feature (Feature(GameName))
import Terminal.ExecGameCommands
    ( execGameCommands, confirmCommand )
--import Terminal.ExecGameCommands (confirmCommand)

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
mainLoop :: GameData -> IO ()
mainLoop gd = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case validateGameCommand input of
        Right err -> do
            print err
            mainLoop gd
        Left gc -> case gc of
            (Play (Numeric i)) -> do
                playGame i
                mainLoop gd
        -- Create Command
            (Create (Text gm) flg) ->
                if null gd
                    then
                        do
                            let gce = [GCEffect { se = "Created game: " ++ gm, ve = "Created game: " ++ gm, gcErr = [] }]
                            res <- confirmCommand gc gce flg
                            when res $ editor ((GameName, [Text gm]):gd)
                    else
                        undefined
            _ -> do
                execGameCommands gc
                mainLoop gd

-- Main program entry point
main :: IO ()
main = do
    clearScreen
    putStrLn "Welcome to CardEngine"
    mainLoop []

