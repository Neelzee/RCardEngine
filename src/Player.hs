module Player where

import Card (Card, Deck)

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
    name :: String,
    hand :: [Card],
    moves :: Moves
}

resetMoves :: Player -> Moves -> Player
resetMoves (Player name hand _) moves = Player name hand moves

-- Deals the given amount of cards to each player
deal :: Int -> Deck -> [Player] -> ([Player], Deck)
deal 0 dck plrs = (plrs, dck)
deal n dck plrs = deal (n - 1) (drop n dck) (zipWith (curry dealPlayer) (take n dck) (take n plrs) ++ drop n plrs)


dealPlayer :: (Card, Player) -> Player
dealPlayer (c, Player nm hnd mvs) = Player nm (c:hnd) mvs

hasMove :: Player -> Move -> Bool
hasMove (Player _ _ []) _ = False
hasMove (Player _ _ (x:xs)) m = x == m || hasMove (Player "" [] xs) m 

getMoveFromString :: String -> Move
getMoveFromString _ = Pass False


getMoveFromPlayer :: Player -> Move -> Move
getMoveFromPlayer (Player _ _ []) _ = Pass True
getMoveFromPlayer (Player _ _ (x:xs)) m = if m == x
    then
        x
    else
        getMoveFromPlayer (Player "" [] xs) m