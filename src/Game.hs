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
    gameName :: String
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
    let (plrs', deck') = deal n (deck game) (players game)
    game { players = plrs', deck = deck'}

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
                let (plrs', deck') = deal 3 defaultCardDeck (fromList (map (`resetMoves` standardMoves) plrs))
                return Game {
                    gameName = "Default Game"
                    , deck = drop 1 deck'
                    , pile = [head deck'], players = plrs'
                    , endCon = [defaultWinCon]
                    , winCon = [sortBy (\p1 p2 -> compare (length $ hand p1) (length $ hand p2)) . toList . players]
                    , state = Start
                    , rules = []
                    , actions = []
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