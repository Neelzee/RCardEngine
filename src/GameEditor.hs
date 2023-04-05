module GameEditor where

import Text.Read (readMaybe)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import System.Console.ANSI (clearScreen)
import Data.List (sort, elemIndex, intercalate)
import ParseExpr (GameExpr (..), validateGameExpr)
import Data.Maybe (mapMaybe, fromJust)
import Player (getMoveFromString)
import System.Directory (listDirectory)
import GameExprError (GameError (..))
import Data.Bifunctor (first)

data EditError = OpenGameError String
    | UnknownFeature String
    | InvalidInput String
    | UnknownCommand String String
    | UnknownFlag String String
    | InvalidArgument String String
    | NoGameError String
    | InvalidStatementError String StatementError
    | MissingFeature String
    deriving (Show, Eq)

data StatementError = SyntaxError String
    | InvalidStatement String
    deriving (Show, Eq)


data CommandEffect = CommandEffect {
    short :: String
    , verbose :: String
}
    deriving (Show, Eq)


data Command = Command {
    cmd :: String
    , info :: String
    , example :: String}
    | Create String [String]
    | Edit Int [String]
    | Add Feature String [String]
    | Update Feature String [String]
    | Remove [Feature] [String]
    | Status [String]
    | Save [String]
    | Exit [String]
    | Copy Feature Int [String]
    | Quit [String]
    | Clear [String]
    | Help
    deriving (Show, Eq)

data Feature = WinCon
    | CardSuits
    | CardValues
    | CardNames
    | EndCon
    | AnyTime
    | StartTime
    | PlayerHand
    | PlayerMoves
    | GameName
    | Saved
    deriving (Show, Eq, Enum)



type GameData = [(Feature, String)]


commands :: [Command]
commands =
    [ Command "create <name>" "Creates a new game" "create newgame"
    , Command "edit <number>" "Edits the game with the matching index" "edit 0"
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
        Just nm -> putStr (nm ++ "> ")
        Nothing -> putStr "> "
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
        Left (Edit gn flgs) ->
            case gd of
                [] -> do
                    g <- allGames
                    dl <- loadFeatures gd gn
                    (gd', ecc) <- if gn `elem` [0..length g]
                        then
                            return (loadFeatures gd gn, [CommandEffect { short = "Loaded features from game " ++ (g !! gn), verbose = "Loaded " ++ show (length dl) ++ " features from game " ++ (g !! gn) }])
                        else
                            do
                                putStrLn ("Invalid input, expected a number between 0 and " ++ show (length g))
                                return (return gd, [])
                    case flgs of
                        ["-e", "-verbose"] -> do
                            putStrLn $ unlines $ map verbose ecc
                            gad <- gd'
                            editor gad
                        ["-e"] -> do
                            putStrLn $ unlines $ map short ecc
                            gad <- gd'
                            editor gad
                        _ -> do
                            gad <- gd'
                            editor gad
                _ -> do
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
                            g <- allGames
                            gd' <- loadFeatures [] a
                            (gad, ecc) <- case lookup fet gd' of
                                Just stmt -> do
                                    return (gd ++ [(fet, stmt)], [CommandEffect { short = "Copied feature: " ++ show fet ++ " from " ++ (g !! a)
                                , verbose = "Copied feature: " ++ show fet ++ " : " ++ stmt ++ " , from " ++ (g !! a)}])
                                Nothing -> do
                                    putStrLn ("Feature " ++ show fet ++ " not in file " ++ (g !! a))
                                    return (gd, [])
                            execFlags ecc flgs
                            editor gad
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



execFlags :: [CommandEffect] -> [String] -> IO ()
execFlags xs ys
    | sort ys == ["-e", "-verbose"] = putStrLn $ unlines $ map verbose xs
    | sort ys == ["-e"] = putStrLn $ unlines $ map short xs
    | otherwise = return ()

-- Takes in a worded string
getCommand :: [String] -> Either Command EditError
getCommand xs = case xs of
    ("create":gm:flgs) -> case validateFlags flgs of
        Just flg -> Left (Create gm flg)
        Nothing -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("edit":gn:flgs) -> case (readMaybe gn :: Maybe Int, validateFlags flgs) of
        (Just i, Just glf) -> Left (Edit i glf)
        (Nothing, _) -> Right (InvalidArgument "Invalid argument, expected a number, but got" ("'" ++ gn ++ "'"))
        (_, Nothing) -> Right (UnknownFlag "Unknown flag" ("'" ++ unwords flgs ++ "'"))
    ("add":feature:ys) -> case (validateFeature feature, validateFlags (words (dropWhile (/= '-') (unwords ys)))) of
        (Just f, Just flg) -> case validateStmt (words (takeWhile (/= '-') (unwords ys))) f of
            Left s -> Left (Add f s flg)
            Right es -> Right (InvalidStatementError "Invalid statement" es)
        (Nothing, _) -> Right (InvalidArgument "Invalid argument, expected a feature, but got" ("'" ++ feature ++ "'"))
        (_, Nothing) -> Right (UnknownFlag "Unknown flag" ("'" ++ (dropWhile (/= '-') (unwords ys) ++ "'")))
    ("update":feature:ys) -> case (validateFeature feature, validateFlags (words (dropWhile (/= '-') (unwords ys)))) of
        (Just f, Just flg) -> case validateStmt (words (takeWhile (/= '-') (unwords ys))) f of
            Left s -> Left (Update f s flg)
            Right es -> Right (InvalidStatementError "Invalid statement" es)
        (Nothing, _) -> Right (InvalidArgument "Invalid argument, expected a feature, but got" ("'" ++ feature ++ "'"))
        (_, Nothing) -> Right (UnknownFlag "Unknown flag" ("'" ++ (dropWhile (/= '-') (unwords ys) ++ "'")))
    ("remove":ys) -> case (mapM validateFeature (words (takeWhile (/= '-') (unwords ys))), validateFlags (words (dropWhile (/= '-') (unwords ys)))) of
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
    ("copy":feature:a:flgs) -> case (validateFeature feature, readMaybe a :: Maybe Int, validateFlags flgs) of
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
    Create gm _-> if null f
        then
            Left ([(GameName, gm)], [CommandEffect { short = "Created game: " ++ gm, verbose = "Created game: " ++ gm }])
        else
            Right (OpenGameError "Cannot create a game, already editing one")
    -- Add feature command
    Add fs s _ -> if null f
        then
            Right (NoGameError "No game to add a feature too")
        else
            Left ((fs, s) : f, [CommandEffect { short = "Added " ++ show fs, verbose = "Added feature " ++ show fs ++ ": " ++ s }])
    -- Update feature
    Update fs s _-> if null f
        then
            Right (NoGameError "No game to add a feature too")
        else
            case lookup fs f of
                Just old -> Left ((fs, s) : filter (\(k, _) -> k /= fs) f, [CommandEffect { short  = "Updated " ++ show fs, verbose = "Updated " ++ show fs ++ " from " ++ old ++ " to " ++ s}])
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
                    Left (f ++ [(Saved, "")],[CommandEffect { short = "Saved game data", verbose = "Saved a total of " ++ show (length diff) ++ " new features" }])

                Nothing -> Left (f ++ [(Saved, "")], [CommandEffect { short = "Saved game data", verbose = "Saved a total of " ++ show (length f) ++ " new features" }])
    _ -> Left (f, [])


gameDataStatus :: GameData -> [CommandEffect]
gameDataStatus [] = []
gameDataStatus ((f, s):xs) = CommandEffect { short = show f, verbose = "Feature " ++ show f ++ " : Statement: " ++ s}: gameDataStatus xs


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

-- Checks if the given feature is valid
validateFeature :: String -> Maybe Feature
validateFeature x = case x of
    "WINCON" -> Just WinCon
    "CARD_SUITS" -> Just CardSuits
    "CARD_VALUES" -> Just CardValues
    "CARD_NAMES" -> Just CardNames
    "ENDCON" -> Just EndCon
    "ANYTIME" -> Just AnyTime
    "PLAYER_HAND" -> Just PlayerHand
    "PLAYER_MOVES" -> Just PlayerMoves
    "GAMENAME" -> Just GameName
    "STARTTIME" -> Just StartTime
    _ -> Nothing

-- Checks if the given statement is valud
validateStmt :: [String] -> Feature -> Either String StatementError
validateStmt xs PlayerHand = case (readMaybe (head xs) :: Maybe Int, xs) of
        (Just _, [x]) -> Left x
        (Nothing, _) -> Right (SyntaxError ("Expected number, got '" ++ head xs ++ "' instead."))
        _ -> Right (SyntaxError ("Expected singular number, got '" ++ show xs ++ "' instead."))
validateStmt (x:xs) CardValues = case readMaybe x :: Maybe Int of
    Just _ -> case validateStmt xs CardValues of
        Left s -> Left (x ++ s)
        e -> e
    Nothing -> Right (SyntaxError ("Expected numbers, but got " ++ x ++ " instead"))
validateStmt (x:xs) PlayerMoves = case getMoveFromString x of
    Just _ -> case validateStmt xs PlayerMoves of
        Left s -> Left (x ++ s)
        e -> e
    Nothing -> Right (SyntaxError ("Invalid Move syntax, '" ++ x ++ "'"))
validateStmt xs f
    | f `elem` [CardSuits, CardNames] = validateSimpleStmt xs 0
    | otherwise = case validateStmt' xs 0 of
            Left expr -> if validateGameExpr expr
                then
                    Left (unwords xs)
                else
                    Right (InvalidStatement "Invalid statement")
            Right e -> Right e



validateSimpleStmt :: [String] -> Int -> Either String StatementError
validateSimpleStmt [] _ = Right (SyntaxError "no statement found")
validateSimpleStmt (x:xs) n = case (all (`elem` (['a'..'z'] ++ ['.',','] ++ ['0'..'9'])) x, last x `notElem` [',', '.']) of
    (False, _) -> Right (SyntaxError ("Invalid syntax on line "  ++ show n ++ ", unknown symbol"))
    (_, False) ->  Right (SyntaxError ("Invalid syntax on line "  ++ show n ++ ", invalid symbol at end of line"))
    _ -> validateSimpleStmt xs (n + 1)

validateStmt' :: [String] -> Int -> Either GameExpr StatementError
validateStmt' [] _ = Right (SyntaxError "no statement found")
validateStmt' (x:xs) n = case x of
    "any" -> case validateStmt' xs (n + 1) of
        Left e -> Left (Any e)
        e -> e
    "players" -> case validateStmt' xs (n + 1) of
        Left e -> Left (Players e)
        e -> e
    "isEqual" -> case xs of
        [l, r] -> case (validateStmt' [l] (n + 1), validateStmt' [r] (n + 2)) of
            (Left a, Left b) -> Left (IsEqual a b)
            (Right e, _) -> Right e
            (_, Right e) -> Right e
        lst -> Right (InvalidStatement ("Statement is complete with " ++ head lst ++ " and " ++ (lst !! 1) ++ " but statement continues with " ++ unwords lst))
    "score" -> case xs of
        [] -> Left Score
        _ -> Right (InvalidStatement ("Statement is complete at " ++ show n ++ ", with " ++ x ++ ", but continues with " ++ unwords xs))
    "hand" -> case xs of
        [] -> Left Hand
        _ -> Right (InvalidStatement ("Statement is complete at " ++ show n ++ ", with " ++ x ++ ", but continues with " ++ unwords xs))
    "greatest" -> case validateStmt' xs (n + 1) of
        Left e -> Left (Greatest e)
        e -> e
    "deck" -> case xs of
        [] -> Left Deck
        _ -> Right (InvalidStatement ("Statement is complete at " ++ show n ++ ", with " ++ x ++ ", but continues with " ++ unwords xs))
    "isEmpty" -> case validateStmt' xs (n + 1) of
        Left e -> Left (IsEmpty e)
        e -> e
    "swap" -> case xs of
        [l, r] -> case (validateStmt' [l] (n + 1), validateStmt' [r] (n + 2)) of
            (Left a, Left b) -> Left (Swap a b)
            (Right e, _) -> Right e
            (_, Right e) -> Right e
        lst -> Right (InvalidStatement ("Statement is complete with " ++ head lst ++ " and " ++ (lst !! 1) ++ " but statement continues with " ++ unwords lst))
    "take" -> case xs of
        [l, m, r] -> case (validateStmt' [l] (n + 1), validateStmt' [m] (n + 2), validateStmt' [r] (n + 3)) of
            (Left a, Left b, Left c) -> Left (Take a b c)
            (Right e, _, _) -> Right e
            (_, Right e, _) -> Right e
            (_, _, Right e) -> Right e
        lst -> Right (InvalidStatement ("Statement is complete with " ++ head lst ++ ", " ++ (lst !! 1) ++ ", and " ++ (lst !! 2) ++ " but statement continues with " ++ unwords lst))
    "pile" -> case xs of
        [] -> Left Pile
        _ -> Right (InvalidStatement ("Statement is complete at " ++ show n ++ ", with " ++ x ++ ", but continues with " ++ unwords xs))
    "shuffle" -> case validateStmt' xs (n + 1) of
        Left e -> Left (Shuffle e)
        e -> e
    "always" -> case xs of
        [] -> Left Always
        _ -> Right (InvalidStatement ("Statement is complete at " ++ show n ++ ", with " ++ x ++ ", but continues with " ++ unwords xs))
    _ -> Right (SyntaxError ("Invalid syntax found at position: " ++ show n))

allGames :: IO [String]
allGames = listDirectory "games"


saveGameData :: GameData -> IO CommandEffect
saveGameData gd = do
    let (_, gd') = span ((/=Saved) . fst) (reverse gd)
    saveGameData gd'

saveGameData' :: GameData -> IO CommandEffect
saveGameData' gd = do
    gl <- allGames
    case lookup GameName gd of
        Just gm -> case elemIndex gm gl of
            Just i -> do
                oldGd <- loadFeatures [] i
                let new = findNew oldGd gd
                let diff = findDiff oldGd gd
                let sim = findSim oldGd gd
                saveGD gd gm
                return (CommandEffect { short = "Overwrote old instance of " ++ gm,
                verbose = "Overwrote " ++ show (length diff) ++ " features, added " ++ show (length new) ++ " features, " ++ show (length sim) ++ " features unchanged"})
            Nothing -> do
                saveGD gd gm
                let new = findNew [] gd
                return (CommandEffect { short = "Saved new instance of " ++ gm,
                verbose = "Saved " ++ show (length new) ++ " features"})
        Nothing -> do
            saveGD gd "new_game"
            let new = findNew [] gd
            return (CommandEffect { short = "Saved new instance of " ++ "new game",
            verbose = "Saved " ++ show (length new) ++ " features"})
    where
        saveGD cs n = writeFile ("games/" ++ n ++ ".txt") (intercalate "\n" (map (\(a, b) -> show a ++ "\n" ++ b ++ "END") cs))

findNew :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
findNew [] _ = []
findNew _ [] = []
findNew old (y:ys) = case lookup (fst y) old of
    Just _ -> findNew old ys
    Nothing -> y : findNew old ys

findDiff :: Eq a => Eq b => [(a, b)] -> [(a, b)] -> [(a, b)]
findDiff [] _ = []
findDiff _ [] = []
findDiff old (y:ys) = case lookup (fst y) old of
    Just o -> if o == snd y
        then
            findDiff old ys
        else
            y : findDiff old ys
    Nothing -> findDiff old ys

findSim :: Eq a => Eq b => [(a, b)] -> [(a, b)] -> [(a, b)]
findSim [] _ = []
findSim _ [] = []
findSim old (y:ys) = case lookup (fst y) old of
    Just o -> if o == snd y
        then
            findSim old ys
        else
            y : findSim old ys
    Nothing -> findSim old ys

loadFeatures :: GameData -> Int -> IO GameData
loadFeatures gd n = do
    g <- allGames
    if n < 0 || n >= length g
    then
        return gd
    else
        do
            content <- readFile ("games/" ++ (g !! n))
            case loadFeatures' gd (lines content) of
                Left gd' -> return gd'
                Right e -> do
                    print e
                    return gd


loadFeatures' :: GameData -> [String] -> Either GameData GameError
loadFeatures' fs d = case parseDataHelper [] d 0 of
    Left d' -> do
        let dt = mapMaybe ((\(mfea, str) -> mfea >>= \fea -> Just (fea, str)) . first validateFeature) d'
        let fs' = mergeList (lookupManyWithKey [WinCon .. Saved] dt) fs
        Left fs'
    Right e -> Right e

parseDataHelper :: [(String, String)] -> [String] -> Int -> Either [(String, String)] GameError
parseDataHelper result [] _ = Left result
parseDataHelper result (x:xs) lineNumber
  | isCommentOrEmptyLine x = parseDataHelper result xs (lineNumber+1)
  | otherwise = case validateFeature x of
      Just rule -> do
        let (linesInStatement, rest) = break isEndStatement xs
        case linesInStatement of
          [] -> Right $ MissingTerminationStatement ("Missing termination statement on line " ++ show lineNumber)
          [statement] -> parseDataHelper ((x, statement):result) (drop 1 rest) (lineNumber + length linesInStatement + 2)
          _ -> Right $ MultipleLinesInStatement ("Multiple lines in statement starting at line " ++ show lineNumber)
      Nothing -> Right $ UnknownKeyWord ("Unknown keyword at line " ++ show lineNumber ++ ", '" ++ x ++ "'")
  where
    isEndStatement line = null line || head (words line) == "END"
    isCommentOrEmptyLine line = null line || head line == '#'


mergeList :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeList [] ys = ys
mergeList ((xk, xv):xs) ys =
  case lookup xk ys of
    Just yv -> mergeList xs ((xk, yv):ys)
    Nothing -> mergeList xs ((xk, xv):ys)



lookupMany :: Eq a => [a] -> [(a, b)] -> [b]
lookupMany keys pairs = mapMaybe (`lookup` pairs) keys

lookupManyWithKey :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupManyWithKey keys list = [(k, v) | (k, v) <- list, k `elem` keys]
