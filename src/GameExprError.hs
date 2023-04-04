module GameExprError where


data GameError = MissingTerminationStatement String | MultipleLinesInStatement String | UnknownKeyWord String deriving (Show)
