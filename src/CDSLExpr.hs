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
        pErr :: CDSLParseErrorCode
        , pExpr :: CDSLExpr
        , rawExpr :: String
    }
    deriving (Show, Eq)

data CDSLParseErrorCode =
    IncompleteExpressionError
    | SyntaxError
    | UnnecessaryOperandError
    | MissingTerminationStatement Int
    | UnknownKeyWord Int
    | OnLoad Feature CDSLParseErrorCode
    deriving (Show, Eq)

data CDSLExecError =
    CDSLExecError
    {
        err :: CDSLExecErrorCode
        , expr :: CDSLExpr
    }
    deriving (Show, Eq)

data CDSLExecErrorCode =
    InvalidSyntaxError
    | SyntaxErrorRightOperand
    | SyntaxErrorLeftOperand
    deriving (Show, Eq)