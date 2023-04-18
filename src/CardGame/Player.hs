module CardGame.Player where

import CardGame.Card ( Card(Card) )
import Data.CircularList (CList, focus, size, rotR, update)
import CDSL.CDSLExpr (CDSLExpr (Text, PlayerAction))
import Data.Maybe (mapMaybe)
import Functions (splitAndTrim, mapCLCount)
import CardGame.PlayerMove (Move (PlayCard, DrawCard, Pass), prettyShow)
import Data.List (intercalate)

data Player = Player {
    name :: String
    , hand :: [Card]
    , moves :: [(Move, Bool)]
    , pScore :: Int
}
    deriving (Show, Eq)

instance Ord Player where
    p1 `compare` p2 = pScore p1 `compare` pScore p2

-- Creates players
createPlayers :: Int -> IO [Player]
createPlayers 0 = return []
createPlayers n = do
    player <- createPlayer
    players <- createPlayers (n - 1)
    return (player : players)

createPlayer :: IO Player
createPlayer = do
    putStrLn "Enter name:"
    n <- getLine
    return (Player n [] [] 0)

resetMoves :: Player -> [(Move, Bool)] -> Player
resetMoves plr mv = plr {moves = mv }

deal :: Int -> [Card] -> CList Player -> (CList Player, [Card])
deal n deck players = deal' (n * size players) deck players

deal' :: Int -> [Card] -> CList Player -> (CList Player, [Card])
deal' 0 dck plrs = (plrs, dck)
deal' _ [] plrs = (plrs, [])
deal' n dck plrs = (giveCards plrs (take n dck), drop n dck) 


giveCards :: CList Player -> [Card] -> CList Player
giveCards plrs [] = plrs
giveCards plrs (x:xs) = case focus plrs of
    Just p -> giveCards (rotR (update (p { hand = x : hand p }) plrs)) xs
    Nothing -> giveCards (rotR plrs) (x:xs)


-- Deals the given card to the given player
dealPlayer :: (Card, Player) -> Player
dealPlayer (c, Player nm hnd mvs s) = Player nm (c:hnd) mvs s

-- Checks if player has the given move
hasMove :: Player -> Move -> Bool
hasMove plr m = case lookup m (moves plr) of
    Just _ -> True
    Nothing -> False

-- Gets move from string
getMoveFromString :: String -> Maybe Move
getMoveFromString "play" = Just PlayCard
getMoveFromString "draw" = Just DrawCard
getMoveFromString "pass" = Just Pass
getMoveFromString _ = Nothing

-- Gets the given move from the player, with the corresponding continuing bool
getMoveFromPlayer :: Player -> Move -> Move
getMoveFromPlayer (Player _ _ [] _) _ = Pass
getMoveFromPlayer (Player _ _ ((x, _):xs) _) m = if m == x
    then
        x
    else
        getMoveFromPlayer (Player "" [] xs 0) m


-- Checks if the typed action is an action the player can do
isValidMove :: String -> Player -> Bool
isValidMove c plr = case getMoveFromString c of
    Just m -> hasMove plr m
    Nothing -> False

standardMoves :: [(Move, Bool)]
standardMoves = [
    (PlayCard, False),
    (DrawCard, True),
    (DrawCard, True),
    (DrawCard, True),
    (Pass, False)]


addScore :: Player -> Card -> Player
addScore (Player nm hnd mvs scr) (Card _ _ s) = Player nm hnd mvs (scr + s)


toString :: (Move, Bool) -> String
toString (a, b) = show a ++ " " ++ (if b then "TRUE" else "FALSE")


parsePlayerMovesExpr :: [CDSLExpr] -> [(Move, Bool)]
parsePlayerMovesExpr [] = []
parsePlayerMovesExpr (Text m:xs) = parsePlayerMoves m ++ parsePlayerMovesExpr xs
parsePlayerMovesExpr (PlayerAction m b:xs) = (m, b) : parsePlayerMovesExpr xs 
parsePlayerMovesExpr (_:xs) = parsePlayerMovesExpr xs



parsePlayerMoves :: String -> [(Move, Bool)]
parsePlayerMoves s = mapMaybe parsePlayerMove (splitAndTrim s)
  

parsePlayerMove :: String -> Maybe (Move, Bool)
parsePlayerMove x = case words x of
    ["PLAY_CARD", b] -> Just (PlayCard, b == "TRUE")
    ["DRAW_CARD", b] -> Just (DrawCard, b == "TRUE")
    ["PASS", b] -> Just (Pass, b == "TRUE")
    _ -> Nothing


prettyPrintMoves :: [(Move, Bool)] -> IO ()
prettyPrintMoves mv = putStrLn (intercalate ", " (map (\(a, _) -> prettyShow a) mv))