module GameCommands where
import CDSLExpr (CDSLExpr (Text))
import Feature (Feature (AnyTime))


data CommandEffect = CommandEffect {
    short :: String
    , verbose :: String
}
    deriving (Show, Eq)

type Flags = [String]


data GameCommand =
    GameCommand
    {
        cmd :: GameCommand
        , info :: String
        , example :: String
    }
    | Create CDSLExpr Flags
    | Save Flags
    | Edit Int Flags
    | Add Feature [CDSLExpr] Flags
    | Update Feature [CDSLExpr] Flags
    | Test Feature [CDSLExpr] Flags
    | Remove [Feature] Flags
    | Copy [Feature] Int Flags
    | Rename Int String Flags
    | Status Flags
    | Exit Flags
    | Quit Flags
    | Clear
    | List Flags
    | Help
    deriving (Eq)


commands :: [GameCommand]
commands =
    [ GameCommand (Create (Text "name") []) "Creates a new game" "create newgame"
    , GameCommand (Edit 0 []) "Edits the game with the matching index" "edit 0"
    , GameCommand (List []) "List the available games to edit" "list"
    , GameCommand (Add AnyTime [] []) "Adds a feature to the game thats currently being edited" "add WINCON greates player score"
    , GameCommand (Update AnyTime [] []) "Updates the feature to the game thats currently being edited" "update WINCON greates player score"
    , GameCommand (Remove [] []) "Removes the feature if it exists" "remove WINCON ENDCON"
    , GameCommand (Status []) "List the features on the current game" "status"
    , GameCommand (Save []) "Saves the changes that has been done on the current game" "save"
    , GameCommand (Exit []) "Exits the editing mode of the current game, unsaved changes will be lost" "exit"
    , GameCommand (Copy [] 0 []) "Copies a feature from a game, and adds it to the active game data" "cp WINCON 0"
    , GameCommand (Quit []) "Quits the program" "quit"
    , GameCommand (List []) "List the games already created" "list"
    , GameCommand Clear "Clears the terminal" "clear"
    , GameCommand Help "Print the help message" "help"
    ]