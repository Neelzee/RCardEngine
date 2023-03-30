module Game where


import Player
import Card

import Data.CircularList
import Text.Read

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