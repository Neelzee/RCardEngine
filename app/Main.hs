module Main where

import Data.List (sortOn)
import System.Directory (listDirectory, renameFile)
import System.Console.ANSI (clearScreen)
import Control.Monad (unless)
import System.IO ( hFlush, stdout )
import CardGame.PlayGame (gameStart)
import GameEditor (Command(..), editor)
import Text.Read (readMaybe)
import GameData.GD (GameData)
import Terminal.GameCommands
import Terminal.ExecGameCommands
import Terminal.ValidateGameCommands
import CDSL.CDSLExpr ( CDSLExpr(Numeric) )

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
            _ -> do
                gd' <- execGameCommands gd gc
                mainLoop gd'

-- Main program entry point
main :: IO ()
main = do
    clearScreen
    putStrLn "Welcome to CardEngine"
    mainLoop []

