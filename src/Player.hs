module Player where

import Card

-- Valid moves, bool for if the move ends the turn or not
data Move = PlayCard Bool | DrawCard Bool | Pass Bool
    deriving (Show)


instance Eq Move where
    x == y = case (x, y) of
        (PlayCard _, PlayCard _) -> True
        (DrawCard _, DrawCard _) -> True
        (Pass _, Pass _) -> True
        _ -> False

type Moves = [Move]

data Player = Player {
    name :: String
    , hand :: [Card]
    , moves :: Moves
    , score :: Int
}
    deriving (Show, Eq)

instance Ord Player where
    p1 `compare` p2 = score p1 `compare` score p2

-- Creates players
createPlayers :: Int -> IO [Player]
createPlayers 0 = return []
createPlayers n = do
    player <- createPlayer
    players <- createPlayers (n - 1)
    return (player : players)

createPlayer :: IO Player
createPlayer = do
    name <- getLine
    return (Player name [] [] 0)

resetMoves :: Player -> Moves -> Player
resetMoves (Player name hand _ n) moves = Player name hand moves n

-- Deals the given amount of cards to each player
deal :: Int -> Deck -> [Player] -> ([Player], Deck)
deal 0 dck plrs = (plrs, dck)
deal n dck plrs = deal (n - 1) (drop n dck) (zipWith (curry dealPlayer) (take n dck) (take n plrs) ++ drop n plrs)

-- Deals the given card to the given player
dealPlayer :: (Card, Player) -> Player
dealPlayer (c, Player nm hnd mvs s) = Player nm (c:hnd) mvs s

-- Checks if player has the given move
hasMove :: Player -> Move -> Bool
hasMove (Player _ _ [] _) _ = False
hasMove (Player _ _ (x:xs) _) m = x == m || hasMove (Player "" [] xs 0) m

-- Gets move from string
getMoveFromString :: String -> Maybe Move
getMoveFromString "play" = Just (PlayCard False)
getMoveFromString "draw" = Just (DrawCard False)
getMoveFromString "pass" = Just (Pass False)
getMoveFromString _ = Nothing

-- Gets the given move from the player, with the corresponding continuing bool
getMoveFromPlayer :: Player -> Move -> Move
getMoveFromPlayer (Player _ _ [] _) _ = Pass True
getMoveFromPlayer (Player _ _ (x:xs) _) m = if m == x
    then
        x
    else
        getMoveFromPlayer (Player "" [] xs 0) m


-- Checks if the typed action is an action the player can do
isValidMove :: String -> Player -> Bool
isValidMove c plr = case getMoveFromString c of
    Just m -> hasMove plr m
    Nothing -> False

standardMoves :: Moves
standardMoves = [
    PlayCard False,
    DrawCard True,
    DrawCard True,
    DrawCard True,
    Pass False]


addScore :: Player -> Card -> Player
addScore (Player nm hnd mvs scr) (Card _ _ s) = Player nm hnd mvs (scr + s)


