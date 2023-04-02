import Data.List (minimumBy, sortOn)
import Data.Ord (comparing)
import System.Directory (listDirectory)
import System.Console.ANSI (clearScreen)

data Command = Command
  { name :: String
  , info :: String
  , example :: String
  }

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
  let distance word1 word2 = levenshteinDistance word1 word2
      closeWords = filter (\w -> distance w word <= 2) words
  in case closeWords of
       [] -> Nothing
       _ -> Just $ take 10 $ map fst $ sortOn snd $ map (\w -> (w, distance w word)) closeWords


-- Suggest a correction for the given command
suggestCorrection :: String -> [Command] -> Maybe String
suggestCorrection input cmds = do
  case findMatches input (map name cmds) of
    Just (x:_) -> Just x
    _ -> Nothing 


-- Print the help message
printHelp :: IO ()
printHelp = mapM_ printCommand commands
  where
    printCommand cmd = putStrLn $ name cmd ++ ":\n  " ++ info cmd ++ "\n  Example: " ++ example cmd

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
    then do
      let filename = "games/" ++ games !! n
      contents <- readFile filename
      putStrLn contents
    else putStrLn "Invalid game number."

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

-- List of available commands
commands :: [Command]
commands =
  [ Command "list" "List the available games" "list"
  , Command "game <number>" "Play the game with the given number" "game <number>"
  , Command "help" "Print the help message" "help"
  , Command "clear" "Clears the terminal" "clear"
  , Command "quit" "Quits the program" "quit"
  ]

-- Main program loop
mainLoop :: IO ()
mainLoop = do
  putStrLn "Welcome to the game selector. Enter a command:"
  input <- getLine
  c <- executeCommand input
  if c
    then
      return ()
    else
      mainLoop

-- Main program entry point
main :: IO ()
main = mainLoop
