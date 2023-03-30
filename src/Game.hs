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

dealCards :: Game -> Int -> Game
dealCards game@(Game dck pl plrs st r ec) n = do
    let (deck', players) = deal n (deck game) (fromList (players game))
    Game deck' pl players' st r ec

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
                return (Game newDeck [] (fromList plrs) Start 0 defaultWinCon)
        _ -> do
            putStrLn "Invalid Input, expected an integer"
            createGame


defaultWinCon :: Game -> Bool
defaultWinCon (Game _ _ plrs _ _ _) = case defaultWinCon' (toList plrs) of
    Just _ -> True
    _ -> False

defaultWinCon' :: [Player] -> Maybe Player
defaultWinCon' [] = Nothing
defaultWinCon' (p@(Player _ [] _):ps) = Just p
defaultWinCon' (_:ps) = defaultWinCon' ps

-- Returns once a player turn is over
doPlayerTurn :: Game -> IO Game
doPlayerTurn game = case focus (players game) of
    Just player -> do
        putStrLn (show (name player) ++ "'s turn!")
        print (hand player)
        putStrLn ("Choose an action: " ++ show (unique (moves player)))
        action <- getLine
        if not (isValidMove c plr) || not (hasMove plr (getMoveFromString c))
                then
                    do
                        putStrLn ("Invalid move, expected " ++ show (unique (moves plr)))
                        sleep 1
                        gameLoop' players deck pile n
                else
                    case getMoveFromPlayer plr (getMoveFromString c) of
                        PlayCard b -> doPlayerActionPlayCard game player b
                        DrawCard b -> doPlayerActionDrawCard game player b
                        Pass b -> doPlayerActionPass game player b

    _ -> error "No players??"

-- Play Card
doPlayerActionPlayCard :: Game -> Player -> Bool -> IO Game
doPlayerActionPlayCard game plr continue = do
    if null (hand plr)
        then
            do
                putStrLn "No cards on hand"
                sleep 1
                game
        else  -- Code to play a card
            do
                putStrLn ("Choose a card: (1/" ++ show (length (hand plr)) ++ ")")
                i <- getLine
                case readMaybe i :: Maybe Int of
                    Just cardIndex -> do
                        if cardIndex `elem` [1..(length (hand plr))]
                            then
                                do
                                    undefined
                            else
                                putStrLn ("Invalid input " ++ i ++ ", expected " ++ show [1..(length (hand plr))])
                                doPlayerActionPlayCard game plr continue
                        let card = hand plr !! cardIndex
                        -- checks if card can be placed
                        if canPlaceCard card (head pile) sortingRules
                            then
                                do
                                    putStrLn ("Plays " ++ show card ++ " on " ++ show (head pile))
                                    let plr' = Player (name plr) (removeFirst (hand plr) card) (removeFirst (moves plr) (PlayCard b))
                                    let pile' = card:pile
                                    -- If player can play card again, 
                                    if b
                                        then
                                            do
                                                sleep 2
                                                game' <- updatePile game pile'
                                                doPlayerTurn (updatePlayers game' (update plr' (players game')))
                                        else
                                            do
                                                sleep 2
                                                game' <- updatePile game pile'
                                                return (updatePlayers game' (rotR (update plr' (players game'))))
                            else
                                do
                                    putStrLn "Cannot place that card"
                                    sleep 1
                                    doPlayerActionPlayCard game plr continue
                    _ -> do
                        putStrLn ("Invalid input " ++ i ++ ", expected and integer in " ++ show [1..(length (hand plr))])
                        doPlayerActionPlayCard game plr continue