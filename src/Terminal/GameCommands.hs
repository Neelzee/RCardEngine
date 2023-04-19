module Terminal.GameCommands (
    GCEffect (..)
    , GameCommand (..)
    , GCError (..)
    , showAll
    , commands
    , Flag
) where


import CDSL.CDSLExpr (CDSLExpr (Text, Null), CDSLExecError, CDSLParseError)
import Feature (Feature (AnyTime))
import Data.List (intercalate)



data GCEffect =
    GCEffect
    {
        se :: String
        , ve :: String
        , gcErr :: [GCError]
    }

type Flag = [String]


data GameCommand =
    GameCommand
    {
        cmd :: GameCommand
        , info :: String
        , example :: String
    }
    -- Creates a new game
    | Create CDSLExpr Flag
    -- Saves the current state of the game
    | Save Flag
    -- Edits an already existing game
    | Edit CDSLExpr Flag
    -- Adds a feature too the current game
    | Add Feature [CDSLExpr] Flag
    -- Updates an exisiting feature
    | Update Feature [CDSLExpr] Flag
    -- Tests the input
    | Test Feature [CDSLExpr] Flag
    -- Removes an existing feaure from the game
    | Remove [Feature] Flag
    -- Copies a feature from one game, to the current one
    | Copy CDSLExpr [Feature] Flag
    -- Renames an existing game
    | Rename CDSLExpr CDSLExpr Flag
    -- Gives the status of the current game
    | Status Flag
    -- Closes the current game
    | Close Flag
    -- Quits the program
    | Quit Flag
    -- Clears the terminal
    | Clear
    -- Lists all existing games
    | List Flag
    -- Lits all commands
    | Help
    -- Plays the given game
    | Play CDSLExpr
    deriving (Eq)

instance Show GameCommand where
    show gc = case gc of
        (Create {}) -> "create"
        (Save {}) -> "save"
        (Edit {}) -> "edit"
        (Add {}) -> "add"
        (Update {}) -> "update"
        (Test {}) -> "test"
        (Remove {}) -> "remove"
        (Copy {}) -> "copy"
        (Rename {}) -> "rename"
        (Status {}) -> "status"
        (Close {}) -> "close"
        (Quit {}) -> "quit"
        (List {}) -> "list"
        (Help {}) -> "help"
        (Play {}) -> "play"
        (Clear {}) -> "clear"
        _ -> "'NO SHOW'"


data GCError =
    GCError
    {
        errType :: GCError
        , input :: String
    }
    -- Unknown command, no valid command found
    | UnknownCommandError String
    -- Invalid Arguments entered
    | InvalidCommandArgumentError String
    -- Invald Flags used
    | InvalidFlagsError
    -- Unknown Flag used
    | UnknownFlagsError
    -- Invalid CDSL input
    | CDSLError (Either [CDSLExecError] [CDSLParseError])
    -- Missing or Corrupt data on load
    | MissingOrCorruptDataError String
    | MissingFeatureError Feature
    | OpenGameDataError String
    | NoGameDataError

instance Show GCError where
    show e = case e of
        (UnknownCommandError s) -> "UnknownCommandError: " ++ s
        (InvalidCommandArgumentError s) -> "InvalidCommandArgumentError: " ++ s
        InvalidFlagsError -> "InvalidFlagsError"
        UnknownFlagsError -> "UnknownFlagsError"
        (CDSLError (Left s)) -> "CDSLError->CDSLExecError: " ++ intercalate "," (map show s)
        (CDSLError (Right s)) -> "CDSLError->CDSLParseError: " ++ intercalate "," (map show s)
        (MissingOrCorruptDataError s) -> "MissingOrCorruptData: " ++ s
        (MissingFeatureError f) -> "MissingFeatureError '" ++ show f ++ "'"
        (OpenGameDataError s) -> "OpenGameDataError: '" ++ s ++ "'"
        NoGameDataError -> "NoGameDataError"
        (GCError { errType = GCError {}, input = y}) -> "GCError: " ++ y
        (GCError { errType = x, input = y}) -> show x ++ ", input: '" ++ y ++ "'"


showAll :: GameCommand -> String
showAll c = case c of
    (Create e flg) -> "create " ++ show e ++ " " ++ unwords flg
    (Edit e flg) -> "edit " ++ show e ++ unwords flg
    (List flg) -> "list " ++ unwords flg
    (Add f e flg) -> "add " ++ show f ++ " " ++ intercalate "," (map show e) ++ " " ++ unwords flg
    (Update f e flg) -> "update " ++ show f ++ " " ++ intercalate "," (map show e) ++ " " ++ unwords flg
    (Status flg) -> "status " ++ unwords flg
    (Save flg) -> "save " ++ unwords flg
    (Copy e fs flg) -> "copy " ++ show e ++ " " ++ intercalate "," (map show fs) ++ " " ++ unwords flg
    (Quit flg) -> "quit " ++ unwords flg
    (List flg) -> "list " ++ unwords flg
    Clear -> "clear"
    Help -> "help"
    _ -> error "no showAll for " ++ show c


commands :: [GameCommand]
commands =
    [ GameCommand (Create (Text "name") []) "Creates a new game" "create newgame"
    , GameCommand (Edit Null []) "Edits the game with the matching index" "edit 0"
    , GameCommand (List []) "List the available games to edit" "list"
    , GameCommand (Add AnyTime [] []) "Adds a feature to the game thats currently being edited" "add WINCON greates player score"
    , GameCommand (Update AnyTime [] []) "Updates the feature to the game thats currently being edited" "update WINCON greates player score"
    , GameCommand (Remove [] []) "Removes the feature if it exists" "remove WINCON ENDCON"
    , GameCommand (Status []) "List the features on the current game" "status"
    , GameCommand (Save []) "Saves the changes that has been done on the current game" "save"
    , GameCommand (Copy Null [] []) "Copies a feature from a game, and adds it to the active game data" "cp 0 WINCON"
    , GameCommand (Quit []) "Quits the program" "quit"
    , GameCommand (List []) "List the games already created" "list"
    , GameCommand Clear "Clears the terminal" "clear"
    , GameCommand Help "Print the help message" "help"
    ]