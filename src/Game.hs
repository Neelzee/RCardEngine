module Game where


import Player
import Card

import Data.CircularList
import Text.Read (readMaybe)
import System.Time.Extra ( sleep )
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import GameRules (GameRule)

data GameState = Start | TurnStart | TurnEnd | RoundStart | RoundEnd | End
    deriving (Show, Eq)

data Game = Game {
    cards :: [Card]
    , deck :: [Card]
    , pile :: [Card]
    , players :: CList Player
    , endCon :: [Game -> Bool]
    , winCon :: [Game -> [Player]]
    , state :: GameState
    , rules :: [(GameRule, String)]
    , actions :: [(GameState, [Game -> Game])]
    , rounds :: Int
}


incrementRoundCounter :: Game -> Game
incrementRoundCounter g = g { rounds = rounds g + 1}

dealCards :: Game -> Int -> Game
dealCards game n = do
    let (plrs', deck') = deal n (deck game) (toList (players game))
    game { players = fromList plrs', deck = deck'}

-- Replaces the deck with a new deck in the game
updateDeck :: Game -> Deck -> Game
updateDeck game dck = game { deck = dck }

-- Replaces the pile with the given pile
updatePile :: Game -> Deck -> Game
updatePile game pl = game { pile = pl }

-- Replaces the players with the updated players
updatePlayers :: Game -> CList Player -> Game
updatePlayers game plrs = game { players = plrs }

updateState :: Game -> GameState -> Game
updateState game st = game { state = st }

createGame :: IO Game
createGame = do
    putStrLn "How many players?"
    c <- getLine
    case readMaybe c :: Maybe Int of
        Just playerCount ->
            do
                plrs <- createPlayers playerCount
                let (plrs', deck') = deal 3 defaultCardDeck (map (`resetMoves` standardMoves) plrs)
                return Game {
                    cards = defaultCardDeck, deck = drop 1 deck'
                    , pile = [head deck'], players = fromList plrs'
                    , endCon = [defaultWinCon]
                    , winCon = [sortBy (\p1 p2 -> compare (length $ hand p1) (length $ hand p2)) . toList . players]
                    , state = Start
                    , rules = []
                    , rounds = 0
                    }
        _ -> do
            putStrLn "Invalid Input, expected an integer"
            createGame

-- Returns true if any player has no cards on their hand
defaultWinCon :: Game -> Bool
defaultWinCon game = case emptyHandEndCon (toList (players game)) of
    Just _ -> True
    _ -> False

emptyHandEndCon :: [Player] -> Maybe Player
emptyHandEndCon [] = Nothing
emptyHandEndCon (p@(Player _ [] _ _):_) = Just p
emptyHandEndCon (_:ps) = emptyHandEndCon ps

highestScore :: [Player] -> Player
highestScore ps = head (quicksort ps)


-- Returns once a player turn is over
doPlayerTurn :: Game -> IO Game
doPlayerTurn game = case focus (players game) of
    Just player -> do
        let game' = updateState game TurnStart
        let playerInfo = playerTurnStart player
        let pileInfo = "Pile: " ++ show (head (pile game))
        let terminal = playerInfo ++ pileInfo
        putStrLn terminal
        action <- getLine
        if not (isValidMove action player)
            then
                do
                    putStrLn ("Invalid move, expected " ++ show (unique (moves player)))
                    sleep 1
                    doPlayerTurn game
            else
                case getMoveFromString action of
                    Just move -> case move of
                        PlayCard -> doPlayerActionPlayCard game' player (lookupOrDefault move False (moves player))
                        DrawCard -> doPlayerActionDrawCard game' player (lookupOrDefault move False (moves player))
                        Pass -> doPlayerActionPass game' player (lookupOrDefault move False (moves player))
                    Nothing -> do
                        putStrLn ("Invalid move, expected " ++ show (unique (moves player)))
                        sleep 1
                        doPlayerTurn game
    _ -> error "No players??"

-- Play Card
doPlayerActionPlayCard :: Game -> Player -> Bool -> IO Game
doPlayerActionPlayCard game plr continue = do
    if null (hand plr)
        then
            do
                putStrLn "No cards on hand"
                sleep 1
                return game
        else  -- Code to play a card
            do
                putStrLn ("Choose a card: (1/" ++ show (length (hand plr)) ++ ")")
                i <- getLine
                case i of
                    "q" -> doPlayerTurn game
                    _ -> do
                            case readMaybe i :: Maybe Int of
                                Just cardIndex -> do
                                    if cardIndex `elem` [1..(length (hand plr))]
                                        then
                                            do
                                                let card = hand plr !! cardIndex
                                                -- checks if card can be placed
                                                if canPlaceCard card (head (pile game)) sortingRules
                                                    then
                                                        do
                                                            putStrLn ("Plays " ++ show card ++ " on " ++ show (head (pile game)))
                                                            let plr' = Player (name plr) (removeFirst (hand plr) card) (removeFirst (moves plr) (PlayCard, continue)) (pScore plr)
                                                            let pile' = card:pile game
                                                            -- If player can play card again, 
                                                            if continue
                                                                then
                                                                    do
                                                                        sleep 2
                                                                        let game' = updatePile game pile'
                                                                        doPlayerTurn (updatePlayers game' (update plr' (players game')))
                                                                else
                                                                    do
                                                                        sleep 2
                                                                        let game' = updateState (updatePile game pile') TurnEnd
                                                                        return (updatePlayers game' (rotR (update plr' (players game'))))
                                                    else
                                                        do
                                                            putStrLn "Cannot place that card"
                                                            sleep 1
                                                            doPlayerActionPlayCard game plr continue
                                        else
                                            do
                                                putStrLn ("Invalid input " ++ show i ++ ", expected " ++ show [1..(length (hand plr))])
                                                doPlayerActionPlayCard game plr continue
                                _ -> do
                                    putStrLn ("Invalid input " ++ show i ++ ", expected and integer in " ++ show [1..(length (hand plr))])
                                    doPlayerActionPlayCard game plr continue

-- Draw card
doPlayerActionDrawCard :: Game -> Player -> Bool -> IO Game
doPlayerActionDrawCard game plr continue = do
    if null (deck game)
        then
            do
                putStrLn "Deck is empty"
                sleep 1
                doPlayerTurn game
        else
            do
                let card = head (deck game)
                putStrLn ("Drew " ++ show card)
                sleep 1
                let plr' = Player (name plr) (card:hand plr) (removeFirst (moves plr) (DrawCard, continue)) (pScore plr)
                let game' = updateDeck game (drop 1 (deck game))
                sleep 1
                if continue
                    then
                        do
                            doPlayerTurn (updatePlayers game' (update plr' (players game')))
                    else
                        do
                            return (updateState (updatePlayers game' (rotR (update plr' (players game')))) TurnEnd)

-- Pass action
doPlayerActionPass :: Game -> Player -> Bool -> IO Game
doPlayerActionPass game plr continue = do
    sleep 1
    let plr' = Player (name plr) (hand plr) (removeFirst (moves plr) (Pass, continue))  (pScore plr)
    if continue
        then
            do
                doPlayerTurn (updatePlayers game (update plr' (players game)))
        else
            do
                return (updatePlayers game (rotR (update plr' (players game))))


-- Removes the first instance of the given element
removeFirst :: Eq a => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) y = if x == y
    then
        xs
    else
        x : removeFirst xs y


sortingRules :: Deck
sortingRules = reverse defaultCardDeck


-- Removes all duplicate elements from the given list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if x `elem` xs
    then
        unique xs
    else
        x : unique xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
    where
        lesser = filter (< x) xs
        greater = filter (>= x) xs


-- Used at the start of a turn
playerTurnStart :: Player -> String
playerTurnStart p = name p ++ "'s turn!\n" ++
    "Hand:\n" ++ show (hand p) ++ "\n\n" ++
    "Choose an action: " ++ show (unique (moves p)) ++ "\n"

-- Clears the terminal by printing
clearTerminal :: IO ()
clearTerminal = putStrLn "\ESC[2J"

-- Sleeps for the given time, counting down
sleepPrint :: String -> Int -> IO ()
sleepPrint s 0 = do
    clearTerminal
    putStrLn s
sleepPrint s n = do
    clearTerminal
    putStrLn s
    putStrLn ("\n" ++ show n)
    sleep 1
    sleepPrint s (n - 1)

lookupOrDefault :: Eq k => k -> a -> [(k, a)] -> a
lookupOrDefault key def assocList = fromMaybe def (lookup key assocList)