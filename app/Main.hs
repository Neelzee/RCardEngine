module Main where

import Data.List (sortOn)
import System.Directory (listDirectory, renameFile)
import System.Console.ANSI (clearScreen)
import Control.Monad (unless)
import System.IO ( hFlush, stdout )
import PlayGame (gameStart)
import GameEditor (Command(..))


-- Compute the Levenshtein distance between two strings
levenshteinDistance :: Eq a => [a] -> [a] -> Int
levenshteinDistance xs ys = lev (length xs) (length ys)
        where
                lev i 0 = i
                lev 0 j = j
                lev i j
                        | xs !! (i - 1) == ys !! (j - 1) = lev (i - 1) (j - 1)
                        | otherwise = minimum [1 + lev i (j - 1), 1 + lev (i - 1) j, 1 + lev (i - 1) (j - 1)]


findMatches :: String -> [String] -> Maybe [String]
findMatches word words =
        do
                let distance word1 word2 = levenshteinDistance word1 word2
                let closeWords = filter (\w -> distance w word <= 2) words
                case closeWords of
                        [] -> Nothing
                        _ -> Just $ take 10 $ map fst $ sortOn snd $ map (\w -> (w, distance w word)) closeWords


-- Suggest a correction for the given command
suggestCorrection :: String -> [Command] -> Maybe String
suggestCorrection input cmds = do
        case findMatches input (map cmd cmds) of
                Just (x:_) -> Just x
                _ -> Nothing


-- Print the help message
printHelp :: IO ()
printHelp = mapM_ printCommand commands
        where
                printCommand c = putStrLn $ cmd c ++ ":\n  " ++ info c ++ "\n  Example: " ++ example c

-- List the available games
listGames :: IO ()
listGames = do
        games <- listDirectory "games"
        putStrLn "Available games:"
        mapM_ putStrLn $ zipWith (\n game -> show n ++ ". " ++ game) [0..] games

-- Play the selected game
playGame :: Int -> IO ()
playGame n = do
    games <- listDirectory "games"
    if n >= 0 && n < length games
        then
            do
            let filename = games !! n
            gameStart filename
        else
            putStrLn "Invalid game number."

-- Parse and execute a command
executeCommand :: String -> IO Bool
executeCommand input = case words input of
	["list"] -> do
		listGames
		return False
	["game", nStr] -> do
		let n = read nStr
		playGame n
		return False
	["rename", nStr, newname] -> do
		let n = read nStr
		renameGame n newname
		return False
	["help"] -> do
		printHelp
		return False
	["quit"] -> return True
	["clear"] -> do
		clearScreen
		return False
	_ -> do
		let possibleCorrection = suggestCorrection (head $ words input) commands
		case possibleCorrection of
				Just correction -> putStrLn $ "\"" ++ input ++ "\" is not a valid command. Did you mean: " ++ correction ++ "?"
				Nothing -> putStrLn $ "\"" ++ input ++ "\" is not a valid command."
		return False


renameGame :: Int -> String -> IO ()
renameGame idx newName = do
	games <- listDirectory "games"
	let filename = games !! idx
	renameFile ("games/" ++ filename) ("games/" ++ newName ++ ".txt")
	putStrLn ("renamed " ++ filename ++ " to " ++ newName ++ ".txt")


-- List of available commands
commands :: [Command]
commands =
        [ Command "list" "List the available games" "list"
        , Command "game <number>" "Play the game with the given number" "game <number>"
		, Command "rename <number> <new name>" "Renames the game with the given new name" "rename 0 pong"
        , Command "clear" "Clears the terminal" "clear"
        , Command "quit" "Quits the program" "quit"
        , Command "help" "Print the help message" "help"
        ]

-- Main program loop
mainLoop :: IO ()
mainLoop = do
        putStr "> "
        hFlush stdout
        input <- getLine
        c <- executeCommand input
        unless c mainLoop

-- Main program entry point
main :: IO ()
main = do
        clearScreen
        putStrLn "Card Game Engine!"
        putStrLn "\n\n\n"
        mainLoop
        
