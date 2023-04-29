module CDSL.CDSLExpr (
    CDSLExpr (..)
    , CDSLParseError (..)
    , CDSLParseErrorCode (..)
    , CDSLExecError (..)
    , CDSLExecErrorCode (..)
    , CardEffect (..)
    , infoCDSL
    , showF
) where

import Feature (Feature, Attribute)
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
    -- Nothing happens
    | Blank
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
    -- Discard pile
    | Discard
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
    | TOLeft
    | TORight
    -- References Card Rank
    | CardRank
    -- References Card Suit
    | CardSuit
    -- References Card Value
    | CardValue
    -- References Player Action
    | PlayerAction Move Bool
    | PAPass
    | PADraw
    | PAPlay
    -- CardEffect
    | CEffect CardEffect [Card]
    -- Current Player
    | CurrentPlayer CDSLExpr
    -- Previous Player
    | PreviousPlayer CDSLExpr
    -- Resets a player ability
    | Reset CDSLExpr
    -- List of cards
    | Cards [Card]
    -- References the turnsystem
    | Turn
    -- Makes the turn go back :)
    | GoBack CDSLExpr
    | GoForward CDSLExpr
    -- Evals too true, if the Current Players last move was the given move
    | IsMove CDSLExpr
    -- Moves to the player
    | PMoves
    -- checks if the given list has the is same
    | IsSame CDSLExpr CDSLExpr
    -- Takes the given amount of elements out of alist
    | Look CDSLExpr CDSLExpr
    -- Puts the elements in the list, into the other list
    | Put CDSLExpr CDSLExpr
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
    | OnValidateFeature Feature [CDSLExecError]
    | OnValidateExpressions (Maybe Feature) [CDSLParseError]
    | NotAFeatureError
    | NotAnAttributeError
    | MissMatchFeatureError
    | NotAFeatureOfAttribute Attribute Feature
    | InvalidFeatureArgumentError
    | NotACardFieldError
    | ParseErrorOnLine CDSLParseErrorCode Int
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
    | UnknownExpressionError
    deriving (Show, Eq)



infoCDSL :: [(CDSLExpr, String)]
infoCDSL =
    [ (Any Null, "Returns true, if any of the elements in a list evaluates to true")
        , (All Null, "Returns a list of all the elements in a list that fullfills a predicate")
        , (Greatest Null, "Returns a list thats sorted by greatest value first, where the comparator depends on context")
        , (Players Null, "List of all the players in the game")
        , (Score, "The score field of a player")
        , (Hand, "The card a player has")
        , (IsEqual Null Null, "Compares two expressions, and checks if they are equal, the comparator depends on context")
        , (IsEmpty Null, "Checks if a given list is empty, and evaluates to true if it is")
        , (If [] [], "An if-statement, checks if the expressions on the left side of ':', all evaluate to true, and if so, executes all the expressions on the left side, in the order they are specified")
        , (Swap Null Null, "Swaps the elements between two lists")
        , (Shuffle Null, "Shuffles the elements of a given list")
        , (Deck, "The deck in a game, where the player draws cards from")
        , (Pile, "The pile in a game, where the player plays their card too")
        , (Discard, "The discard pile in a game, where the player can discard their cards too")
        , (Take Null Null Null, "Takes the specified amount of elements, or all, from one list, and adds them to another")
        , (Always, "Evaluates to true")
        , (Never, "Evaluates to false")
        , (Not [], "Negates the expressions")
        , (And [] [], "Evaluates too true if both expressions evaluates to true")
        , (Or [] [], "Evaluates too true if any expressions evaluates to true")
        , (AffectPlayer Blank, "Specifies an action that will affect the current player")
        , (TOLeft, "References the turn ordering, will start with the first player, then go to the last, second last, etc")
        , (TORight, "References the turn ordering, will start with the first player, then second, then third, etc")
        , (CardSuit, "References the suit of a card")
        , (CardRank, "References the rank of a card")
        , (CardValue, "References the value of a card")
        , (CEffect Blank [], "Specifies what cards should have what card effects")
        , (CurrentPlayer Null, "Is the current player, i.e. the one whos turn it is")
        , (Reset Null, "Resets a players fields or values")
        , (Cards [], "List of cards")
        , (Turn, "A reference too the turn-system of the game")
        , (GoBack Null, "Will change the current player, to be a reference the previous player, and therefore make it that players turn again")
        , (GoForward Null, "Will change the current player, to be a reference the next player, and therefore make it that players turn")
        , (IsMove Null, "Will evaluate too true, if the last move a player made, is the specified move")
        , (PMoves, "The moves a player can make")
        , (IsSame Null Null, "Evalutes to true, if the specified list of cards, all have the same specified card field")
        , (Look Null Null, "Gets an immutable references to the specified amount of elements from a list")
        , (Put Null Null, "Puts all the elements from one list, into the other")
        , (CLe, "Lesser comparator")
        , (CGr, "Greater comparator")
        , (CEq, "Equal comparator")
        , (CLEq, "Lesser and Equal comparator")
        , (CGRq, "Greater and Equal comparator")
    ]


showF :: CDSLExpr -> String
showF (All _) = "all"
showF (Any _) = "any"
showF (Greatest _) = "greatest"
showF (Players _) = "players"
showF Score = "score"
showF Hand = "hand"
showF (IsEqual _ _) = "isEqual"
showF (Numeric _) = "numeric"
showF (IsEmpty _) = "isEmpty"
showF (If _ _) = ":"
showF (Swap _ _) = "swap"
showF (Shuffle _) = "shuffle"
showF Deck = "deck"
showF Pile = "pile"
showF (Take {}) = "take"
showF Always = "always"
showF Never = "never"
showF (Not _) = "not"
showF (And _ _) = "and"
showF (Or _ _) = "or"
showF CardRank = "rank"
showF CardSuit = "suit"
showF CardValue = "value"
showF (PlayerAction _ _) = "playerAction"
showF (AffectPlayer _) = "affectPlayer"
showF (CEffect _ _) = "cardEffect"
showF (Reset _) = "reset"
showF (CurrentPlayer _) = "currentPlayer"
showF PMoves = "pMoves"
showF (Cards _) = "cards"
showF CLe = "cle"
showF CGr = "cgr"
showF CGRq = "cgrq"
showF CLEq = "cleq"
showF CEq = "ceq"
showF TOLeft = "left"
showF TORight = "right"
showF Discard = "discard"
showF (IsSame _ _) = "isSame"
showF (Look _ _) = "look"
showF (Put _ _) = "put"
showF (IsMove _) = "isMove"
showF PAPass = "pPass"
showF PADraw = "pDraw"
showF PAPlay = "pPlay"
showF _ = "(NOT ADDED)"
