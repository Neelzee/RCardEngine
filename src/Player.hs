module Player where

import Card
import Data.CircularList (CList, focus, size, rotR, update)

-- Valid moves, bool for if the move ends the turn or not
data Move = PlayCard | DrawCard | Pass
    deriving (Eq)

instance Show Move where
    show PlayCard = "PLAYCARD"
    show DrawCard = "DRAWCARD"
    show Pass = "PASS"

type Moves = [Move]

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
deal' n (c:dck) plrs = case focus plrs of
    Just p -> deal' (n - 1) dck (rotR (update (p { hand = c:hand p}) plrs))
    Nothing -> deal' n (c:dck) (rotR plrs)





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