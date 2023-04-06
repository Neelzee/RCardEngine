{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module ParseCardDSL where
import Text.Read (readMaybe)
import Data.List (groupBy)
import Data.Either (partitionEithers)
import Game (Game (deck, pile, players))
import Card (Card)
import Player (Player(hand, name, pScore))
import Data.CircularList (toList)


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


data CDSLExecError =
    CDSLExecError
    {
        err :: CDSLExecError
        , expr :: CDSLExpr
    }
    | InvalidSyntaxError
    | SyntaxErrorRightOperand
    | SyntaxErrorLeftOperand


-- Checks if a given CDSLExpr is valid
validateCDSLExpression :: CDSLExpr -> Either CDSLExpr CDSLExecError
validateCDSLExpression e@(Any (Players (IsEmpty Hand))) = Left e
validateCDSLExpression e@(All (Players (IsEmpty Hand))) = Left e
validateCDSLExpression e@(Any (Players (IsEqual a (Numeric _)))) = if a == Hand || a == Score
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLExpression e@(Any (Players (IsEqual (Numeric _) a))) = if a == Hand || a == Score
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })

validateCDSLExpression e@(All (Players (IsEqual a (Numeric n)))) = if a == Hand || a == Score
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLExpression e@(All (Players (IsEqual (Numeric _) a))) = if a == Hand || a == Score
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLExpression e@(Shuffle a) = if a == Deck || a == Pile || a == Players TurnOrder
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLExpression e@(Greatest (Players a)) = if a == Score || a == Hand
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLExpression e@(IsEmpty a) = if a == Deck || a == Pile
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
validateCDSLExpression e@(Swap a b) = if (a == Pile || a == Deck) && (b == Pile || b == Deck)
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e})
validateCDSLExpression e@(Take (Numeric _) f t) = if (f == Pile || f == Deck) && (t == Pile || t == Deck)
    then
        Left e
    else
        Right (CDSLExecError { err = InvalidSyntaxError, expr = e})
validateCDSLExpression (And l r) = case (execCDSLBool l, execCDSLBool r) of
    (Left _, Left _) -> Left (And l r)
    (Right er, _) -> Right er
    (_, Right er) -> Right er
validateCDSLExpression (Or l r) = case (execCDSLBool l, execCDSLBool r) of
    (Left _, Left _) -> Left (Or l r)
    (Right er, _) -> Right er
    (_, Right er) -> Right er
validateCDSLExpression Always = Left Always
validateCDSLExpression Never = Left Never
validateCDSLExpression e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })


execCDSLBool :: CDSLExpr -> Either Bool CDSLExecError
execCDSLBool Always = Left True
execCDSLBool Never = Left False
execCDSLBool (IsEqual (Numeric a) (Numeric b)) = Left (a == b)
execCDSLBool (Not ex) = case execCDSLBool ex of
    Left b -> Left (not b)
    e -> e
execCDSLBool (Or l r) = case (execCDSLBool l, execCDSLBool r) of
    (Left lt, Left rt) -> Left (lt || rt)
    (Right e, _) -> Right e
    (_, Right e) -> Right e
execCDSLBool (And l r) = case (execCDSLBool l, execCDSLBool r) of
    (Left lt, Left rt) -> Left (lt && rt)
    (Right e, _) -> Right e
    (_, Right e) -> Right e
execCDSLBool e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })


execCDSLGameBool :: CDSLExpr -> Game -> Either Bool CDSLExecError
execCDSLGameBool (IsEmpty Deck) g = Left (null (deck g))
execCDSLGameBool (IsEmpty Pile) g = Left (null (pile g))
execCDSLGameBool e@(IsEqual a b) g = case (fromCDSLToCard a, fromCDSLToCard b) of
    (Just fa, Just fb) -> Left (fa g == fb g)
    (Nothing, _) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = e })
    (_, Nothing) -> Right (CDSLExecError { err = SyntaxErrorRightOperand, expr = e })
execCDSLGameBool (Any (Players (IsEmpty Hand))) g = Left (any (null . hand) (toList (players g)))
execCDSLGameBool (All (Players (IsEmpty Hand))) g = Left (all (null . hand) (toList (players g)))
execCDSLGameBool e@(Any (Players (IsEqual a b))) g = case ((a == Score, b == Score), (execCDSLInt a, execCDSLInt b)) of
    ((True, _), (_, Left n)) -> Left (any ((==n) . pScore) (toList (players g)))
    ((_, True), (Left n, _)) -> Left (any ((==n) . pScore) (toList (players g)))
    ((False, _), (_, Right _)) -> Right (CDSLExecError { err = InvalidSyntaxError, expr = a })
    ((_, False), (Right _, _)) -> Right (CDSLExecError { err = InvalidSyntaxError, expr = b })
    _ -> Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
execCDSLGameBool e _ = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })

isCDSLExprNumeric :: CDSLExpr -> Bool
isCDSLExprNumeric (Numeric _) = True
isCDSLExprNumeric _ = False

execCDSLInt :: CDSLExpr -> Either Int CDSLExecError
execCDSLInt (Numeric n) = Left n
execCDSLInt e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })

fromCDSLToCard :: CDSLExpr -> Maybe (Game -> [Card])
fromCDSLToCard x = case x of
    Pile -> Just pile
    Deck -> Just deck
    _ -> Nothing



-- Parses a words line to an CDSLExpression, or gives an error
-- Only converts a string too CDSLExpr, does not check if its a valid statement
parseCDSLFromString :: [String] -> Either CDSLExpr CDSLParseError
parseCDSLFromString [] = Left Null
parseCDSLFromString (x:xs) = case x of
    "any" -> case parseCDSLFromString xs of
        Left exr -> Left (Any exr)
        e -> e
    "all" -> case parseCDSLFromString xs of
        Left exr -> Left (All exr)
        e -> e
    "greatest" -> case parseCDSLFromString xs of
        Left exr -> Left (Greatest exr)
        e -> e
    "players" -> case parseCDSLFromString xs of
        Left exr -> Left (Players exr)
        e -> e
    "score" -> if null xs
        then
            Left Score
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Score, rawExpr = x})
    "hand" -> if null xs
        then
            Left Hand
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Hand, rawExpr = x})
    "isEqual" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (IsEqual l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = IsEqual (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = IsEqual l (pExpr e) })
            (Right e, _) -> Right e
            (_, Right e) -> Right e
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = IsEqual Null Null, rawExpr = x})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = IsEqual Null Null, rawExpr = x})
    "isEmpty" -> case parseCDSLFromString xs of
        Left exr -> Left (IsEmpty exr)
        e -> e
    "swap" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (Swap l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = Swap (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = Swap l (pExpr e) })
            (Right l, _) -> Right l
            (_, Right r) -> Right r
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Swap Null Null, rawExpr = x})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Swap Null Null, rawExpr = x})
    "shuffle" -> case parseCDSLFromString xs of
        Left exr -> Left (Shuffle exr)
        e -> e
    "deck" -> if null xs
        then
            Left Deck
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Deck, rawExpr = x})
    "pile" -> if null xs
        then
            Left Pile
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Pile, rawExpr = x})
    "take" -> if length xs <= 2
        then
            Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Take Null Null Null, rawExpr = x})
        else
            if length xs < 3
                then
                    Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Take Null Null Null, rawExpr = x})
                else
                    case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1], parseCDSLFromString [xs !! 2]) of
                        (Left i, Left f, Left t) -> Left (Take i f t)
                        (Right e, _, _) -> Right e
                        (_, Right e, _) -> Right e
                        (_, _, Right e) -> Right e
    "always" -> if null xs
        then
            Left Always
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Always, rawExpr = x})
    "never" -> if null xs
        then
            Left Never
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Never, rawExpr = x})
    "not" -> case parseCDSLFromString xs of
        Left exr -> Left (Not exr)
        e -> e
    "and" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (And l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = And (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = And l (pExpr e) })
            (Right l, _) -> Right l
            (_, Right r) -> Right r
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = And Null Null, rawExpr = x})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = And Null Null, rawExpr = x})
    "or" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (Or l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = Or (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = Or l (pExpr e) })
            (Right l, _) -> Right l
            (_, Right r) -> Right r
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Or Null Null, rawExpr = x})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Or Null Null, rawExpr = x})
    "rank" -> if null xs
        then
            Left CardRank
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = CardRank, rawExpr = x})
    "suit" -> if null xs
        then
            Left CardSuit
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = CardSuit, rawExpr = x})
    "value" -> if null xs
        then
            Left CardValue
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = CardValue, rawExpr = x})
    "turnOrder" -> if null xs
        then
            Left TurnOrder
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = TurnOrder, rawExpr = x})
    _ -> case readMaybe x :: Maybe Int of
        Just i -> Left (Numeric i)
        _ -> Right (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = x})



parseIFCDSLFromString :: String -> Either CDSLExpr CDSLParseError
parseIFCDSLFromString xs = do
    case processIfString xs of
        ([], _) -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = If [Null] [Null], rawExpr = xs})
        (_, []) -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = If [Null] [Null], rawExpr = xs})
        (conds, exr) -> case (partitionEithers (map parseCDSLFromString conds), partitionEithers (map parseCDSLFromString exr)) of
            ((cs, []), (es, [])) -> Left (If cs es)
            ((_, e@CDSLParseError {}:_), (es, [])) -> Right (e { pExpr = If [Null] es })
            ((cs, []), (_, e@CDSLParseError {}:_)) -> Right (e { pExpr = If cs [Null] })
            (_, (_, e@CDSLParseError {}:_)) -> Right (e { pExpr = If [Null] [Null] })
            ((_, e@CDSLParseError {}:_), _) -> Right (e { pExpr = If [Null] [Null] })
            _ -> Right (CDSLParseError { pErr = SyntaxError, pExpr = If [Null] [Null], rawExpr = xs})





processIfString :: String -> ([[String]], [[String]])
processIfString str =
  let (left, right) = break (==":") (words str)
      leftGroups = groupBy (\x y -> y /= "," && y /= ":") left
      rightGroups = groupBy (\x y -> y /= ",") (tail right)
  in (leftGroups, rightGroups)

