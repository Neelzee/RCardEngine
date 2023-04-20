module CDSL.CDSLExpr (
    CDSLExpr (..)
    , CDSLParseError (..)
    , CDSLParseErrorCode (..)
    , CDSLExecError (..)
    , CDSLExecErrorCode (..)
    , CardEffect (..)
) where

import Feature (Feature)
import CardGame.PlayerMove (Move)
import CardGame.Card (Card)

data CardEffect =
    -- Choose a new card to change to, i.e. standard "Vri-Åttern"
    ChangeCard [CDSLExpr]
    -- Swap hand with another Player
    | SwapHand
    -- Takes a Card from an Player's Hand
    | TakeFromHand
    -- Gives a Card too another Player
    | GiveCard
    -- Passes the next Player's round
    | PassNext
    -- Makes the Player draw the given amount of cards, and skip their turn
    | DrawCards Int
    deriving (Show, Eq)

-- Card Domain Specific Language
data CDSLExpr =
    -- ATM, only used to signal Any Player
    Any CDSLExpr
    -- Only used to signal All Players
    | All CDSLExpr
    -- Should return the greatest of a list
    | Greatest CDSLExpr
    -- List of all Players
    | Players CDSLExpr
    -- Score of a Player
    | Score
    -- The hand of a player
    | Hand
    -- Evals to true if two expressions are equal
    | IsEqual CDSLExpr CDSLExpr
    -- Numeric expression
    | Numeric Int
    -- Evals to true if a given list is empty
    | IsEmpty CDSLExpr
    -- If, a list of expression all evals too true, then execute a list of expressions
    | If [CDSLExpr] [CDSLExpr]
    -- Swap the data in two different lists
    | Swap CDSLExpr CDSLExpr
    -- Shuffle a list
    | Shuffle CDSLExpr
    -- References the Game Deck
    | Deck
    -- References the Game Pile
    | Pile
    -- Takes a given number from one list, and appends it to the other
    | Take CDSLExpr CDSLExpr CDSLExpr
    -- Evals to true
    | Always
    -- Evals to false
    | Never
    -- Negates an boolean
    | Not [CDSLExpr]
    -- Evals too true if both conditions are true
    | And [CDSLExpr] [CDSLExpr]
    -- Evals too true if any condition is true
    | Or [CDSLExpr] [CDSLExpr]
    -- The following expression affects the next player
    | AffectPlayer CardEffect
    -- References the turn order
    | TurnOrder
    -- References Card Rank
    | CardRank
    -- References Card Suit
    | CardSuit
    -- References Card Value
    | CardValue
    -- References Player Action
    | PlayerAction Move Bool
    -- CardEffect
    | CEffect CardEffect [Card]
    -- Current Player
    | CurrentPlayer CDSLExpr
    -- Resets a player ability
    | Reset CDSLExpr
    -- List of cards
    | Cards [Card]
    -- Moves
    | PMoves
    -- A string
    | Text String
    -- Lesser
    | CLe
    -- Greater
    | CGr
    -- Equal
    | CEq
    -- Lesser Or Equal
    | CLEq
    -- Greater or Equal
    | CGRq
    -- Null value, is not used in normal expressions, and just in error messages
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
    | NotIfStatementError
    | SyntaxError
    | UnnecessaryOperandError
    | MissingTerminationStatement Int
    | UnknownKeyWord Int
    | OnLoad Feature CDSLParseErrorCode
    | NotAFeatureError
    | MissMatchFeatureError
    | InvalidFeatureArgumentError
    | NotACardFieldError
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
    | InvalidBoolEvaluationError
    deriving (Show, Eq)