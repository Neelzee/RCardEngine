# Haskell-Project

This is a Card Game Engine, for the terminal.
One can create Card Games for terminals, in a terminal.

# CDSL

Card Domain Specific Language
<br>
The scripting language used to create card games.
Lets talk about some of the keywords in CDSL.
<br>

Certain keywords in this scripting language, can only be used in certain contexts.
For example both, `any` and `all` are keywords specific for `players`, which itself references a list of all the players playing the card game.
This can be used to create end and/or win conditions for your game.
An example would be, the game ends once any player has an empty hand.
```
any players hand isEmpty
```
In this context, `hand` references the hand for each player, the cards they are holding, so if any player has an empty hand, this would evaluate to true.

An example of a win condition, using `all`, could be this:
```
all players score isEqual 10
```
Which would return a list, of all the players whose score is equal too 10.

One could also make conditional statements, that are check at specific points in the game.

Lets say, that we want to check if the deck, where players draw their cards, is empty, at the start of each new turn.
Then we need to add it to the list of CDSL-statements too be executed at turn start.
This is how we would do that:
```
turn_start = isEmpty deck : [swap pile deck, take 1 deck pile, shuffle deck]
```

Here, several things are happening.
the use of `:`, indicates that this is an if-statement, where the expression on the left side of `:`, is the condition, and the expressions on the right side, are the statements to be executed, if the condition evaluates too true.

so, if the `deck` is empty, i.e. it's just an empty list, then we execute the following statments, in order.
First, we `swap` `pile` and `deck`. `swap` is a keyword, that can swap two lists that contains the same type, in this case, cards. 
`pile` is a reference too the list that contains the cards that the players play.
So, we take the cards from `pile` and place them in `deck`.
Then, we `take` a single item, in this case a card, from `deck`, which now has all the cards in play, except the ones in the players hands, and we place this card in the `pile`, this would be the same card that was on the top, so from a players point of view, nothing would be different, except there would now be cards in `deck`. We then `shuffle` the `deck`.

# How to use

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