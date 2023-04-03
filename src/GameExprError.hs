module GameExprError where


data GameError = GameError String | GameInvalidSyntax String
    deriving (Show, Eq)