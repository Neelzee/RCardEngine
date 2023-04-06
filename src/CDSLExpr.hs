module CDSLExpr where
import Feature (Feature)


data KeyWord = CardValues
    | CardRanks
    | CardSuits
    | WinCondition
    | EndCondition
    | CardConstraints
    | PlayerMoves
    | PlayerHandCount
    | AnyTime
    | StartTime
    | CardEffects

data CDSLExpr =
    Any CDSLExpr
    | All CDSLExpr
    | Greatest CDSLExpr
    | Players CDSLExpr
    | Score
    | Hand
    | IsEqual CDSLExpr CDSLExpr
    | Numeric Int
    | IsEmpty CDSLExpr
    | If [CDSLExpr] [CDSLExpr]
    | Swap CDSLExpr CDSLExpr
    | Shuffle CDSLExpr
    | Deck
    | Pile
    | Take CDSLExpr CDSLExpr CDSLExpr
    | Always
    | Never
    | Not CDSLExpr
    | And CDSLExpr CDSLExpr
    | Or CDSLExpr CDSLExpr
    | TurnOrder
    | CardRank
    | CardSuit
    | CardValue
    | Text String
    | Null
    deriving (Show, Eq)

data CDSLParseError =
    CDSLParseError
    {
        pErr :: CDSLParseError
        , pExpr :: CDSLExpr
        , rawExpr :: String
    }
    | IncompleteExpressionError
    | SyntaxError
    | UnnecessaryOperandError
    | MissingTerminationStatement String
    | UnknownKeyWord String
    | OnLoad Feature CDSLParseError
    deriving (Show, Eq)

data CDSLExecError =
    CDSLExecError
    {
        err :: CDSLExecError
        , expr :: CDSLExpr
    }
    | InvalidSyntaxError
    | SyntaxErrorRightOperand
    | SyntaxErrorLeftOperand
    deriving (Show, Eq)