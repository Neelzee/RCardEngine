module Game where


import Player
import Card

import Data.CircularList
import Text.Read (readMaybe)
import System.Time.Extra ( sleep )

data GameState = Start | Turn | End
    deriving (Show, Eq)

data Game = Game {
    deck :: Deck
    , pile :: Deck
    , players :: CList Player
    , state :: GameState
    , rounds :: Int
    , endCon :: Game -> Bool
}


incrementRoundCounter :: Game -> Game
incrementRoundCounter (Game dck pl plrs st r ec) = (Game dck pl plrs st (r + 1) ec)

dealCards :: Game -> Int -> Game
dealCards game@(Game dck pl plrs st r ec) n = do
    let (plrs', deck') = deal n (deck game) (toList (players game))
    Game deck' pl (fromList plrs') st r ec

-- Replaces the deck with a new deck in the game
updateDeck :: Game -> Deck -> Game
updateDeck (Game _ pl plrs st r ec) dck = Game dck pl plrs st r ec

-- Replaces the pile with the given pile
updatePile :: Game -> Deck -> Game
updatePile (Game dck _ plrs st r ec) pl = Game dck pl plrs st r ec

-- Replaces the players with the updated players
updatePlayers :: Game -> CList Player -> Game
updatePlayers (Game dck pl _ st r ec) plrs = Game dck pl plrs st r ec

createGame :: IO Game
createGame = do
    putStrLn "How many players?"
    c <- getLine
    case readMaybe c :: Maybe Int of
        Just playerCount ->
            do
                plrs <- createPlayers playerCount
                let (plrs', deck') = deal 3 newDeck (map (\p -> resetMoves p standardMoves) plrs)
                return (Game (drop 1 deck') [head deck'] (fromList plrs') Start 0 defaultWinCon)
        _ -> do
            putStrLn "Invalid Input, expected an integer"
            createGame

-- Returns true if any player has no cards on their hand
defaultWinCon :: Game -> Bool
defaultWinCon (Game _ _ plrs _ _ _) = case defaultWinCon' (toList plrs) of
    Just _ -> True
    _ -> False

defaultWinCon' :: [Player] -> Maybe Player
defaultWinCon' [] = Nothing
defaultWinCon' (p@(Player _ [] _ _):ps) = Just p
defaultWinCon' (_:ps) = defaultWinCon' ps




-- Returns once a player turn is over
doPlayerTurn :: Game -> IO Game
doPlayerTurn game = case focus (players game) of
    Just player -> do
        putStrLn (show (name player) ++ "'s turn!")
        putStrLn "\n"
        putStrLn "\n"
        putStrLn ("Choose an action: " ++ show (unique (moves player)))
        putStrLn "\n"
        putStrLn "\n"
        putStrLn ("Pile: " ++ show (head (pile game)))
        putStrLn "\n"
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
                        PlayCard b -> doPlayerActionPlayCard game player b
                        DrawCard b -> doPlayerActionDrawCard game player b
                        Pass b -> doPlayerActionPass game player b
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
                putStrLn "Hand: "
                print (hand player)
                putStrLn "\n"
                putStrLn ("Choose a card: (1/" ++ show (length (hand plr)) ++ ")")
                i <- getLine
                case i of
                    "cancel" -> doPlayerTurn game
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
                                                            let plr' = Player (name plr) (removeFirst (hand plr) card) (removeFirst (moves plr) (PlayCard continue)) (score plr)
                                                            let pile' = card:(pile game)
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
                                                                        let game' = updatePile game pile'
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
                let plr' = Player (name plr) (card:hand plr) (removeFirst (moves plr) (DrawCard continue)) (score plr)
                let game' = updateDeck game (drop 1 (deck game))
                sleep 1
                if continue
                    then
                        do
                            doPlayerTurn (updatePlayers game' (update plr' (players game')))
                    else
                        do
                            return (updatePlayers game' (rotR (update plr' (players game'))))

-- Pass action
doPlayerActionPass :: Game -> Player -> Bool -> IO Game
doPlayerActionPass game plr continue = do
    sleep 1
    let plr' = Player (name plr) (hand plr) (removeFirst (moves plr) (Pass continue))  (score plr)
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
sortingRules = reverse newDeck


-- Removes all duplicate elements from the given list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if x `elem` xs
    then
        unique xs
    else
        x : unique xs