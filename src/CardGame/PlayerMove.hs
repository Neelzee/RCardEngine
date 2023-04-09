module CardGame.PlayerMove where

-- Valid moves, bool for if the move ends the turn or not
data Move = PlayCard | DrawCard | Pass
    deriving (Eq)

instance Show Move where
    show PlayCard = "PLAY_CARD"
    show DrawCard = "DRAW_CARD"
    show Pass = "PASS"

prettyShow :: Move -> String
prettyShow m = case m of
    PlayCard -> "play"
    DrawCard -> "draw"
    Pass -> "pass"