module GameEditor where

import Text.Read (readMaybe)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import System.Console.ANSI (clearScreen)
import Data.List (sort, intercalate, partition, groupBy)
import System.Directory (listDirectory)
import Control.Monad (zipWithM)
import Feature (Feature (Saved, GameName), fromStringToFeature)
import Constants (gameFolder)
import Data.Either (partitionEithers)
import SaveGD (saveGameData)
import Commands (CommandEffect (short, verbose, CommandEffect))
import GD (GameData)
import CDSLExpr (CDSLExpr(Text, Null), CDSLParseError)
import ParseCardDSL (fromCDSLToString, parseCDSLFromString)
import LoadGD (loadGameData)

data EditError = OpenGameError String
    | UnknownFeature String
    | InvalidInput String
    | UnknownCommand String String
    | UnknownFlag String String
    | InvalidArgument String String
    | NoGameError String
    | InvalidStatementError String [CDSLParseError]
    | MissingFeature String
    deriving (Show, Eq)

data StatementError = SyntaxError String
    | InvalidStatement String
    deriving (Show, Eq)


data Command = Command
    {
        cmd :: String
        , info :: String
        , example :: String
    }
    | Create CDSLExpr Flags
    | Edit Int Flags
    | Add Feature [CDSLExpr] Flags
    | Update Feature [CDSLExpr] Flags
    | Remove [Feature] Flags
    | Status Flags
    | Save Flags
    | Exit Flags
    | Copy Feature Int Flags
    | Quit Flags
    | Clear Flags
    | List Flags
    | Help
    deriving (Show, Eq)

type Flags = [String]


commands :: [Command]
commands =
    [ Command "create <name>" "Creates a new game" "create newgame"
    , Command "edit <number>" "Edits the game with the matching index" "edit 0"
    , Command "list" "List the available games to edit" "list"
    , Command "add <feature> <statement>" "Adds a feature to the game thats currently being edited" "add WINCON greates player score"
    , Command "update <feature> <statement>" "Updates the feature to the game thats currently being edited" "update WINCON greates player score"
    , Command "remove <feature>" "Removes the feature if it exists" "remove WINCON ENDCON"
    , Command "status" "List the features on the current game" "status"
    , Command "save" "Saves the changes that has been done on the current game" "save"
    , Command "exit" "Exits the editing mode of the current game, unsaved changes will be lost" "exit"
    , Command "cp <feature> <from>" "Copies a feature from a game, and adds it to the active game data" "cp WINCON 0"
    , Command "quit" "Quits the program" "quit"
    , Command "clear" "Clears the terminal" "clear"
    , Command "help" "Print the help message" "help"
    ]

flags :: [String]
flags = ["-verbose" -- Turns on makes the message verbose
    , "-e" -- shows the effect of the command
    ]

getFlag :: Command -> [String]
getFlag (Create _ fls) = fls
getFlag (Edit _ fls) = fls
getFlag (Add _ _ fls) = fls
getFlag (Update _ _ fls) = fls
getFlag (Remove _ fls) = fls
getFlag (Status fls) = fls
getFlag (Save fls) = fls
getFlag (Exit fls) = fls
getFlag (Copy _ _ fls) = fls
getFlag (Quit fls) = fls
getFlag (Clear fls) = fls
getFlag _ = []


printTable :: [(String, String, String)] -> IO ()
printTable rows = do
  let (col1, col2, col3) = unzip3 rows
      max1 = maximum (map length col1)
      max2 = maximum (map length col2)
      max3 = maximum (map length col3)
      formatRow (a, b, c) = intercalate "  " [pad max1 a, pad max2 b, pad max3 c]
      pad len str = str ++ replicate (len - length str) ' '
      table = map formatRow rows
      separator = replicate (length (head table)) '-'
  putStrLn separator
  mapM_ putStrLn table
  putStrLn separator



printCommands :: [Command] -> IO ()
printCommands xs = printTable (pc xs)
    where
        pc [] = []
        pc ((Command nm inf ex):ys) = (nm, inf, ex) : pc ys
        pc (_:ys) = pc ys


editor :: GameData -> IO ()
editor gd = do
    case lookup GameName gd of
        Just (Text nm:_) -> putStr ("edit -> " ++ nm ++ " > ")
        _ -> putStr "edit> "
    hFlush stdout
    c <- getLine
    case getCommand (words c) of
        Left (Quit _) -> return ()
        Left Help -> do
            printCommands commands
            editor gd
        Left (Clear _) -> do
            clearScreen
            editor gd
        -- Edit command
        Left (Edit gn flgs) -> if null gd
            then
                do
                    g <- allGames
                    if gn `elem` [0..length g]
                        then
                            do
                                dl <- loadGameData gd gn
                                case dl of
                                    Left gd' -> do
                                        let ecc = [CommandEffect { short = "Loaded features from game " ++ (g !! gn), verbose = "Loaded " ++ show (length gd') ++ " features from game " ++ (g !! gn) }]
                                        execFlags ecc flgs
                                        editor gd'
                                    Right err -> do
                                            execFlags (map fromCDSLParseErrorOnLoad err) flgs
                                            editor gd
                        else
                            do
                                execFlags [CommandEffect { short = "Invalid number", verbose = "Invalid number, expected a number between 0 and " ++ show (length g) }] flgs
                                editor gd
            else
                do
                putStrLn "Cannot load while other game is active"
                editor gd
         -- copy
        Left (Copy fet a flgs) ->
            case gd of
                [] -> do
                    putStrLn "No game to copy too"
                    editor gd
                _ -> do
                    g <- allGames
                    if a `elem` [0..(length g)]
                    then
                        do
                            res <- loadGameData [] a
                            case res of
                                Left gd' -> do
                                    (gad, ecc) <- case lookup fet gd' of
                                        Just stmt -> do
                                            return (gd ++ [(fet, stmt)], [CommandEffect { short = "Copied feature: " ++ show fet ++ " from " ++ (g !! a)
                                        , verbose = "Copied feature: " ++ show fet ++ " : " ++ intercalate "," (map fromCDSLToString stmt) ++ " , from " ++ (g !! a)}])
                                        Nothing -> do
                                            putStrLn ("Feature " ++ show fet ++ " not in file " ++ (g !! a))
                                            return (gd, [])
                                    execFlags ecc flgs
                                    editor gad
                                Right err -> do
                                    execFlags (map fromCDSLParseErrorOnLoad err) flgs
                                    editor gd
                    else
                        do
                            putStrLn ("Expected a number between 0 and " ++ show (length g) ++ ", in 'from', but got " ++ show a ++ " instead")
                            editor gd
        -- exit
        Left (Exit flgs) -> if null gd
        then
            do
                putStrLn "No game to exit"
                editor gd
        else
            do
                let (diff, _) = span ((/=Saved) . fst) (reverse gd)
                let ecc = [CommandEffect { short = "Exit editing mode", verbose = "Disregareded a total of " ++ show (length diff) ++ " new features" }]
                e <- saveGameData gd
                execFlags (ecc ++ [e]) flgs
                editor []
        -- List games
        Left (List flgs) -> do
            ecc <- listGameData
            execFlags ecc flgs
            editor gd
        Left cm -> case execCommand cm gd of
            Right err -> do
                print err
                editor gd
            Left (gd', ce) -> case getFlag cm of
                ["-e", "-verbose"] -> do
                    putStrLn $ unlines $ map verbose ce
                    editor gd'
                ["-e"] -> do
                    putStrLn $ unlines $ map short ce
                    editor gd'
                _ -> editor gd'
        Right e -> do
            print e
            editor gd


listGameData :: IO [CommandEffect]
listGameData = do
    games <- allGames
    agd <- zipWithM (\ _x i -> loadGameData [] i) games [0..]
    return (listGameData' agd 0)
    where
        listGameData' :: [Either GameData [CDSLParseError]] -> Int -> [CommandEffect]
        listGameData' [] _ = []
        listGameData' (x:xs) n = case x of
            Left gd -> case lookup GameName gd of
                Just (Text gm:_) -> CommandEffect { short = gm, verbose = "Game:\n" ++ gm ++ "\nFeatures:\n" ++ intercalate "\n" (map verbose (gameDataStatus gd)) ++ "\n" } : listGameData' xs (n + 1)
                _ -> CommandEffect { short = "Failed listing game: " ++ show n ++ ", found no name", verbose = "Failed loading game: " ++ show n ++ ", found no name"} : listGameData' xs (n + 1)
            Right e -> CommandEffect { short = "Failed listing game: " ++ show n, verbose = "Failed loading game: " ++ show n ++ ", error: " ++ show e} : listGameData' xs (n + 1)

fromCDSLParseErrorOnLoad :: CDSLParseError -> CommandEffect
fromCDSLParseErrorOnLoad e = CommandEffect { short = "Failed loading game", verbose = "Failed loading game, error: " ++ show e}

execFlags :: [CommandEffect] -> [String] -> IO ()
execFlags xs ys
    | sort ys == ["-e", "-verbose"] = putStrLn $ unlines $ map verbose xs
    | sort ys == ["-e"] = putStrLn $ unlines $ map short xs
    | otherwise = return ()

-- Takes in a worded string
getCommand :: [String] -> Either Command EditError
getCommand xs = case xs of
    ("create":gm:flgs) -> case validateFlags flgs of
        Just flg -> Left (Create (Text gm) flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("edit":gn:flgs) -> case (readMaybe gn :: Maybe Int, validateFlags flgs) of
        (Just i, Just glf) -> Left (Edit i glf)
        (Nothing, _) -> Right (InvalidArgument "Invalid argument, expected a number, but got" ("'" ++ gn ++ "'"))
        (_, Nothing) -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("add":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case partitionEithers (map (parseCDSLFromString . words) exs) of
                (exps, []) -> Left (Add f exps flg)
                (_, errs) -> Right (InvalidStatementError "Invalid statement" errs)
        Nothing -> Right (InvalidArgument "Invalid argument, expected a feature, but got" ("'" ++ feature ++ "'"))
    ("update":feature:ys) -> case fromStringToFeature feature of
        Just f -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case partitionEithers (map (parseCDSLFromString . words) exs) of
                (exps, []) -> Left (Update f exps flg)
                (_, errs) -> Right (InvalidStatementError "Invalid statement" errs)
        Nothing -> Right (InvalidArgument "Invalid argument, expected a feature, but got" ("'" ++ feature ++ "'"))
    ("remove":ys) -> case (mapM fromStringToFeature (words (takeWhile (/= '-') (unwords ys))), validateFlags (words (dropWhile (/= '-') (unwords ys)))) of
        (Just fets, Just flg) -> Left (Remove fets flg)
        (Nothing, _) -> Right (InvalidArgument "Invalid argument, expected a feature, but got" ("'" ++ (takeWhile (/= '-') (unwords ys) ++ "'")))
        (_, Nothing) -> Right (UnknownFlag "Unknown flag" ("'" ++ (dropWhile (/= '-') (unwords ys) ++ "'")))
    ("status":flgs) -> case validateFlags flgs of
        Just flg -> Left (Status flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("save":flgs) -> case validateFlags flgs of
        Just flg -> Left (Save flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("exit":flgs) -> case validateFlags flgs of
        Just flg -> Left (Exit flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("copy":feature:a:flgs) -> case (fromStringToFeature feature, readMaybe a :: Maybe Int, validateFlags flgs) of
        (Just ft, Just i, Just flg) -> Left (Copy ft i flg)
        (Nothing, _, _) -> Right (InvalidArgument "Invalid argument, expected a feature, but got" ("'" ++ feature ++ "'"))
        (_, Nothing, _)-> Right (InvalidArgument "Invalid argument, expected a number, but got" ("'" ++ a ++ "'"))
        (_, _, Nothing)-> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("quit":flgs) -> case validateFlags flgs of
        Just flg -> Left (Quit flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("clear":flgs) -> case validateFlags flgs of
        Just flg -> Left (Clear flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ["help"] -> Left Help
    ("list":flgs) -> case validateFlags flgs of
        Just flg -> Left (List flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    _ -> Right (UnknownCommand "Unknown command or flags" (unwords xs))


validateFlags :: [String] -> Maybe [String]
validateFlags xs = case sort xs of
    ["-e", "-verbose"] -> Just xs
    ["-e"] -> Just xs
    [] -> Just []
    _ -> Nothing

-- Takes in user input
execCommand :: Command -> GameData -> Either (GameData, [CommandEffect]) EditError
execCommand xs f = case xs of
    -- Create Command
    Create (Text gm) _-> if null f
        then
            Left ([(GameName, [Text gm])], [CommandEffect { short = "Created game: " ++ gm, verbose = "Created game: " ++ gm }])
        else
            Right (OpenGameError "Cannot create a game, already editing one")
    -- Add feature command
    Add fs s _ -> if null f
        then
            Right (NoGameError "No game to add a feature too")
        else
            Left ((fs, s) : f, [CommandEffect { short = "Added " ++ show fs, verbose = "Added feature " ++ show fs ++ ":\n" ++ intercalate "\n" (map fromCDSLToString s) }])
    -- Update feature
    Update fs s _-> if null f
        then
            Right (NoGameError "No game to add a feature too")
        else
            case lookup fs f of
                Just old -> Left ((fs, s) : filter (\(k, _) -> k /= fs) f, [CommandEffect { short  = "Updated " ++ show fs, verbose = "Updated " ++ show fs ++ " from\n" ++ intercalate "\n" (map fromCDSLToString old) ++ "\n" ++ " to " ++ intercalate "\n" (map fromCDSLToString s)}])
                Nothing -> Right (MissingFeature ("No feature to update: " ++ show fs))
    -- remove
    Remove features _-> if null f
        then
            Right (NoGameError "No game to remove a feature from")
        else
            case removeFeature f features of
                Left (ecc, fs') -> Left (ecc, fs')
                Right e -> Right e
    -- status
    Status _ -> if null f
      then
          Right (NoGameError "No game to show status off")
      else
          Left (f, gameDataStatus f)
    -- save
    Save _ -> if null f
        then
            Right (NoGameError "No game to save")
        else
            case lookup Saved f of
                Just _ -> do
                    let (diff, _) = span ((/=Saved) . fst) (reverse f)
                    Left (f ++ [(Saved, [Null])],[CommandEffect { short = "Saved game data", verbose = "Saved a total of " ++ show (length diff) ++ " new features" }])

                Nothing -> Left (f ++ [(Saved, [Null])], [CommandEffect { short = "Saved game data", verbose = "Saved a total of " ++ show (length f) ++ " new features" }])
    _ -> Left (f, [])


gameDataStatus :: GameData -> [CommandEffect]
gameDataStatus [] = []
gameDataStatus ((Saved, _):xs) = gameDataStatus xs
gameDataStatus ((GameName, _):xs) = gameDataStatus xs
gameDataStatus ((f, s):xs) = CommandEffect { short = show f, verbose = "Feature " ++ show f ++ " : Statement ->\n" ++ intercalate "\n\t" (map fromCDSLToString s)}: gameDataStatus xs


removeFeature :: GameData -> [Feature] -> Either (GameData, [CommandEffect]) EditError
removeFeature fs xs = removeFeature' fs xs []

removeFeature' :: GameData -> [Feature] -> [CommandEffect] -> Either (GameData, [CommandEffect]) EditError
removeFeature' fs [] ecc = Left (fs, ecc)
removeFeature' [] _ ecc = Left ([], ecc)
removeFeature' fs (f:xs) ecc = case lookup f fs of
        Just _ -> do
            let rs = filter (\(k, _) -> k /= f) fs
            removeFeature' rs xs (CommandEffect ("Removed: " ++ show f) ("Removed: " ++ show f):ecc)
        Nothing -> removeFeature' fs xs (CommandEffect ("No instance of " ++ show f ++ " found.") ("No instance of " ++ show f ++ " found."):ecc)

allGames :: IO [String]
allGames = listDirectory gameFolder

