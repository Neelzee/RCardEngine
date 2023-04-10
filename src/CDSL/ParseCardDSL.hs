module CDSL.ParseCardDSL where

import Text.Read (readMaybe)
import Data.List (groupBy, intercalate)
import Data.Either (partitionEithers)
import CDSL.ExecCDSLExpr (execCDSLBool)
import CDSL.CDSLExpr
import Data.List.Extra (splitOn, trim)
import Data.Text (unpack, strip, pack)
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass))
import Feature (Feature)



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

validateCDSLExpression e@(All (Players (IsEqual a (Numeric _)))) = if a == Hand || a == Score
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

isCDSLExprNumeric :: CDSLExpr -> Bool
isCDSLExprNumeric (Numeric _) = True
isCDSLExprNumeric _ = False


parseCDSLFromStringList :: String -> Either [CDSLExpr] CDSLParseError
parseCDSLFromStringList "" = Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Null, rawExpr = "" })
parseCDSLFromStringList xs = parse (map trim (splitOn "," xs))
    where
        parse :: [String] -> Either [CDSLExpr] CDSLParseError
        parse [] = Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Null, rawExpr = "" })
        parse (x:ys) = case parseIFCDSLFromString x of
            Left e -> case parse ys of
                Left es -> Left (e:es)
                Right er -> Right er
            Right _ -> case parseCDSLFromString (words x) of
                Left e -> case parse ys of
                    Left es -> Left (e:es)
                    Right er -> Right er
                Right er -> Right er


parseCDSLPlayerAction :: String -> Either [CDSLExpr] CDSLParseError
parseCDSLPlayerAction xs = parse (parseStringList xs)
    where
        parse :: [CDSLExpr] -> Either [CDSLExpr] CDSLParseError
        parse [] = Left []
        parse (y:ys) = case y of
            Text txt -> case words txt of
                ("PLAYCARD":b) -> case parse ys of
                    Left ex -> Left (PlayerAction PlayCard (head b == "TRUE") : ex)
                    Right e -> Right e
                ("DRAWCARD":b) -> case parse ys of
                    Left ex -> Left (PlayerAction DrawCard (head b == "TRUE") : ex)
                    Right e -> Right e
                ("PASS":b) -> case parse ys of
                    Left ex -> Left (PlayerAction Pass (head b == "TRUE") : ex)
                    Right e -> Right e
                _ -> Right (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show y })

            _ -> Right (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show y })




parseStringList :: String -> [CDSLExpr]
parseStringList xs = map (Text . trim) (splitOn "," xs)


-- Parses a words line to an CDSLExpression, or gives an error
-- Only converts a string to a CDSLExpr, does not check if its a valid statement
parseCDSLFromString :: [String] -> Either CDSLExpr CDSLParseError
parseCDSLFromString [] = Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Null, rawExpr = "" })
parseCDSLFromString (x:xs) = case x of
    "any" -> case parseCDSLFromString xs of
                Left exr -> Left (Any exr)
                Right e -> Right (e { pExpr = Any (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
    "all" -> case parseCDSLFromString xs of
        Left exr -> Left (All exr)
        Right e -> Right (e { pExpr = All (pExpr e), rawExpr = x ++ " " ++ rawExpr e })
    "greatest" -> case parseCDSLFromString xs of
        Left exr -> Left (Greatest exr)
        Right e -> Right (e { pExpr = Greatest (pExpr e), rawExpr = x ++ " " ++ rawExpr e})
    "players" -> case parseCDSLFromString xs of
        Left exr -> Left (Players exr)
        Right e -> Right (e { pExpr = Players (pExpr e), rawExpr = x ++ " " ++ rawExpr e })
    "score" -> if null xs
        then
            Left Score
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Score, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "hand" -> if null xs
        then
            Left Hand
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Hand, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "isEqual" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (IsEqual l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = IsEqual (pExpr e) r, rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = IsEqual l (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
            (Right e@CDSLParseError {}, _) -> Right (e { pExpr = IsEqual (pExpr e) Null, rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = IsEqual Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = IsEqual Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "isEmpty" -> case parseCDSLFromString xs of
        Left exr -> Left (IsEmpty exr)
        e -> e
    "swap" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (Swap l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = Swap (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = Swap l (pExpr e) })
            (Right l, _) -> Right l
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Swap Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Swap Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "shuffle" -> case parseCDSLFromString xs of
        Left exr -> Left (Shuffle exr)
        Right e -> Right (e { pExpr = Shuffle (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
    "deck" -> if null xs
        then
            Left Deck
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Deck, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "pile" -> if null xs
        then
            Left Pile
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Pile, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "take" -> if length xs <= 2
        then
            Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Take Null Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
        else
            if length xs < 3
                then
                    Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Take Null Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
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
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Always, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "never" -> if null xs
        then
            Left Never
        else
            Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Never, rawExpr = x ++ " " ++ " " ++ unwords xs})
    "not" -> case parseCDSLFromString xs of
        Left exr -> Left (Not exr)
        e -> e
    "and" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (And l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = And (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = And l (pExpr e) })
            (Right l, _) -> Right l
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = And Null Null, rawExpr = x})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = And Null Null, rawExpr = x})
    "or" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (Or l r)
            (Right e@CDSLParseError {}, Left r) -> Right (e { pExpr = Or (pExpr e) r })
            (Left l, Right e@CDSLParseError {}) -> Right (e { pExpr = Or l (pExpr e) })
            (Right l, _) -> Right l
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


fromCDSLToString :: CDSLExpr -> String
fromCDSLToString (All e) = "all " ++ fromCDSLToString e
fromCDSLToString (Any e) = "any " ++ fromCDSLToString e
fromCDSLToString (Greatest e) = "greatest " ++ fromCDSLToString e
fromCDSLToString (Players e) = "players " ++ fromCDSLToString e
fromCDSLToString Score = "Score"
fromCDSLToString Hand = "hand"
fromCDSLToString (IsEqual l r) = "isEqual " ++ fromCDSLToString l ++ fromCDSLToString r
fromCDSLToString (Numeric i) = show i ++ " "
fromCDSLToString (IsEmpty e) = "isEmpty " ++ fromCDSLToString e
fromCDSLToString (If cond stmt) = intercalate "," (map fromCDSLToString cond) ++ " : " ++ intercalate "," (map fromCDSLToString stmt)
fromCDSLToString (Swap l r) = "swap " ++ fromCDSLToString l ++ fromCDSLToString r
fromCDSLToString (Shuffle e) = "shuffle " ++ fromCDSLToString e
fromCDSLToString Deck = "deck"
fromCDSLToString Pile = "pile"
fromCDSLToString (Take c f t) = "take " ++ fromCDSLToString c ++ fromCDSLToString f ++ fromCDSLToString t
fromCDSLToString Always = "always"
fromCDSLToString Never = "never"
fromCDSLToString (Not e) = "not " ++ fromCDSLToString e
fromCDSLToString (And l r) = "and " ++ fromCDSLToString l ++ fromCDSLToString r
fromCDSLToString (Or l r) = "or " ++ fromCDSLToString l ++ fromCDSLToString r
fromCDSLToString TurnOrder = "turnOrder"
fromCDSLToString CardRank = "rank"
fromCDSLToString CardSuit = "suit"
fromCDSLToString CardValue = "value"
fromCDSLToString (Text s) = s
fromCDSLToString (PlayerAction a b) = show a ++ " " ++ if b then "TRUE" else "FALSE"
fromCDSLToString _ = ""


parseIFCDSLFromString :: String -> Either CDSLExpr CDSLParseError
parseIFCDSLFromString xs = if ':' `elem` xs
    then
        case processIfString xs of
            ([], _) -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = If [Null] [Null], rawExpr = xs})
            (_, []) -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = If [Null] [Null], rawExpr = xs})
            (conds, exr) -> case (partitionEithers (map parseCDSLFromString conds), partitionEithers (map parseCDSLFromString exr)) of
                ((cs, []), (es, [])) -> Left (If cs es)
                ((_, e@CDSLParseError {}:_), (es, [])) -> Right (e { pExpr = If [Null] es })
                ((cs, []), (_, e@CDSLParseError {}:_)) -> Right (e { pExpr = If cs [Null] })
                (_, (_, e@CDSLParseError {}:_)) -> Right (e { pExpr = If [Null] [Null] })
    else
        Right (CDSLParseError { pErr = NotIfStatementError, pExpr = Null, rawExpr = xs})




processIfString :: String -> ([[String]], [[String]])
processIfString str = do
    let (left, right) = break (== ':') str
    let trimmedLeft = unpack $ strip $ pack left
    let trimmedRight = unpack $ strip $ pack (drop 1 right)
    let l = map words (splitOn "," trimmedLeft)
    let r = map words (splitOn "," trimmedRight)
    (l, r)


toNumeric :: CDSLExpr -> Maybe CDSLExpr
toNumeric x = case x of
    (Text t) -> case readMaybe t :: Maybe Int of
        Just i -> Just (Numeric i)
        _ -> Nothing
    n@(Numeric _) -> Just n
    _ -> Nothing


parseCDSLF :: String -> Either (Feature, [CDSLExpr]) [CDSLParseError]
parseCDSLF xs = case break (== "=") (words xs) of
     -- validate feature and expressions
    ([f], exprs) -> case (validateFeature f, validateCDSL exprs) of
        (Left feature, Left expressions) -> Left (feature, expressions)
        (Right err, _) -> Right [err]
        (_, Right err) -> Right err
    -- Not a statement error
    _ -> Right [CDSLParseError
        {
            pErr = SyntaxError
            , pExpr = Null
            , rawExpr = xs
        }]
   



validateFeature :: String -> Either Feature CDSLParseError
validateFeature x = case x of
    _ -> undefined

validateCDSL :: [String] -> Either [CDSLExpr] [CDSLParseError]
validateCDSL = undefined