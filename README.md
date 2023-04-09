# Haskell-Project


run with `stack run`

## Commands:

### General commands

Commands that work throughout* the application.

- `quit`
    - Quits the application.

- `clear`
    - Clears the terminal.

- `list`
    - Lists all existing games.

- `help`
    - Displays all commands.


### Main Menu

Entrance for application, where games can be launched and edited from.

- `play <index>`
    - Playes the game, specified with the given index, and enters `play` mode.

- `create <name>`
    - Creates a new game, with the given name, and enters `edit` mode.

- `edit <index>`
    - Edits an already existing game, with the given index number, and enters `edit` mode.

- `rename <index> <new_name>`
    - Renames an already existing game, identified with the given index number, to the specified new name.

### Edit Mode

Where one creates and edits games.

- `save`
    - Saves the game one is currently working on.

- `add <feature> <CDSL>`
    - Adds a feature to the game one is editing, the given CDSL code is validated, before it is being added.

- `update <feature> <CDSL>`
    - Updates an existing feature, and replaces the old CDSL code with the new, given one.

- `test <feature> <CDSL>`
    - Validates the given CDSL code, checking for errors.

- `remove <feature>`
    - Removes the specified feature from the current game.

- `copy <index> <feature>`
    - Copies the list of features, from the game, specified by index.

- `status`
    - Shows the status of the current game, listing all features in it.

- `close`
    - Closes the current game, removing all unsaved features, and returning to the `main menu`.

### Play Mode

- `play <index>`
    - Plays the specified card, identified with the given index.

- `draw <count>`
    - Draws the specified amount of cards.

- `pass`
    - Passes the turn.

- `moves`
    - Lists all valid player moves.

- `hand`
    - Shows the hand of the current player.

- `score`
    - Shows the score of the current player.

- `quit`
    - Quits the game for the current player, skipping the turn onto the next player.

- `help`
    - Lists all commands a player can do.