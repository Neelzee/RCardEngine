module PlayerMove where

-- Valid moves, bool for if the move ends the turn or not
data Move = PlayCard | DrawCard | Pass
    deriving (Eq)

instance Show Move where
    show PlayCard = "PLAYCARD"
    show DrawCard = "DRAWCARD"
    show Pass = "PASS"

