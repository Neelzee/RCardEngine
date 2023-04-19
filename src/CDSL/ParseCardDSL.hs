module CDSL.ParseCardDSL where

import Text.Read (readMaybe)
import Data.List (groupBy, intercalate)
import Data.Either (partitionEithers)
import CDSL.ExecCDSLExpr (execCDSLBool)
import CDSL.CDSLExpr
import Data.List.Extra (splitOn)
import Data.Text (unpack, strip, pack)
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass))
import Feature
import Functions (isList, stringToList, trim, mapIf, removeFirst)
import CardGame.Player (getMoveFromString, parsePlayerMovesExpr, parsePlayerMoves, parsePlayerMove)
import Data.Maybe (mapMaybe)
import CardGame.Card



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
            (Right e, Left r) -> Right (e { pExpr = IsEqual (pExpr e) r, rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
            (Left l, Right e) -> Right (e { pExpr = IsEqual l (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
            (Right e, _) -> Right (e { pExpr = IsEqual (pExpr e) Null, rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e })
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = IsEqual Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = IsEqual Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs})
    "isEmpty" -> case parseCDSLFromString xs of
        Left exr -> Left (IsEmpty exr)
        e -> e
    "swap" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (Swap l r)
            (Right e, Left r) -> Right (e { pExpr = Swap (pExpr e) r })
            (Left l, Right e) -> Right (e { pExpr = Swap l (pExpr e) })
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
    "!" -> case parseCDSLFromString xs of
        Left exr -> Left (Not exr)
        e -> e
    "&&" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (And l r)
            (Right e, Left r) -> Right (e { pExpr = And (pExpr e) r })
            (Left l, Right e) -> Right (e { pExpr = And l (pExpr e) })
            (Right l, _) -> Right l
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = And Null Null, rawExpr = x})
        _ -> Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = And Null Null, rawExpr = x})
    "||" -> case length xs of
        2 -> case (parseCDSLFromString [head xs], parseCDSLFromString [xs !! 1]) of
            (Left l, Left r) -> Left (Or l r)
            (Right e, Left r) -> Right (e { pExpr = Or (pExpr e) r })
            (Left l, Right e) -> Right (e { pExpr = Or l (pExpr e) })
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
fromCDSLToString Score = "score"
fromCDSLToString Hand = "hand"
fromCDSLToString (IsEqual l r) = "isEqual " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString (Numeric i) = show i
fromCDSLToString (IsEmpty e) = "isEmpty " ++ fromCDSLToString e
fromCDSLToString (If cond stmt) = intercalate ", " (map fromCDSLToString cond) ++ " : " ++ intercalate ", " (map fromCDSLToString stmt)
fromCDSLToString (Swap l r) = "swap " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString (Shuffle e) = "shuffle " ++ fromCDSLToString e
fromCDSLToString Deck = "deck"
fromCDSLToString Pile = "pile"
fromCDSLToString (Take c f t) = "take " ++ fromCDSLToString c ++ " " ++ fromCDSLToString f ++ " " ++ fromCDSLToString t
fromCDSLToString Always = "always"
fromCDSLToString Never = "never"
fromCDSLToString (Not e) = "!" ++ fromCDSLToString e
fromCDSLToString (And l r) = "&& " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString (Or l r) = "|| " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString TurnOrder = "turnOrder"
fromCDSLToString CardRank = "rank"
fromCDSLToString CardSuit = "suit"
fromCDSLToString CardValue = "value"
fromCDSLToString (Text s) = s
fromCDSLToString (PlayerAction a b) = show a ++ " " ++ if b then "TRUE" else "FALSE"
fromCDSLToString (AffectPlayer ce) = "affect_player " ++ show ce
fromCDSLToString (CEffect ce cs) = "card_effect " ++ show ce ++ " " ++ show cs
fromCDSLToString (Reset ce) = "reset " ++ fromCDSLToString ce
fromCDSLToString (CurrentPlayer ce) = "player " ++ fromCDSLToString ce
fromCDSLToString PMoves = "moves"
fromCDSLToString _ = "(NOT ADDED)"



parseIFCDSLFromString :: String -> Either CDSLExpr CDSLParseError
parseIFCDSLFromString xs = if ':' `elem` xs
    then
        case processIfString xs of
            ([], _) -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = If [Null] [Null], rawExpr = xs})
            (_, []) -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = If [Null] [Null], rawExpr = xs})
            (conds, exr) -> case (partitionEithers (map parseCDSLFromString conds), partitionEithers (map parseCDSLFromString exr)) of
                ((cs, []), (es, [])) -> Left (If cs es)
                ((_, e:_), (es, [])) -> Right (e { pExpr = If [Null] es })
                ((cs, []), (_, e:_)) -> Right (e { pExpr = If cs [Null] })
                (_, (_, e:_)) -> Right (e { pExpr = If [Null] [Null] })
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

readCDSL :: String -> Either (Feature, [CDSLExpr]) (Maybe Feature, [CDSLParseError])
readCDSL xs = do
    let (y, ys) = (takeWhile (/='=') xs, removeFirst (dropWhile (/='=') xs) '=')
    case validateFeature (unpack $ strip $ pack y) of
        -- Cards
        Left CEDrawCard -> case readMaybe (unwords (drop 1 (words y))) :: Maybe Int of
            Just i -> Left (CardEffects, [CEffect (DrawCards i) (getCards (stringToList ys))])
            Nothing -> Right (Just CardEffects, [CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = show (drop 1 (words y)) }])
        Left CESwapHand -> Left (CardEffects, [CEffect SwapHand (getCards (stringToList ys))])
        Left CEChangeCard -> Left (CardEffects, [CEffect ChangeCard (getCards (stringToList ys))])
        Left CETakeFromHand -> Left (CardEffects, [CEffect TakeFromHand (getCards (stringToList ys))])
        Left CEPassNext -> Left (CardEffects, [CEffect PassNext (getCards (stringToList ys))])
        Left CEGiveCard -> Left (CardEffects, [CEffect GiveCard (getCards (stringToList ys))])

        Left PlayerMoves -> Left (PlayerMoves, map (uncurry PlayerAction) (mapMaybe parsePlayerMove (stringToList ys)))
        Left f -> case parseExpr (stringToList ys) of
            Left exprs -> Left (f, exprs)
            Right err -> Right (Just f, err)
        Right err -> Right (Nothing, [err])

getCards :: [String] -> [Card]
getCards [] = []
getCards (x:xs) = case splitOn "." x of
    [s,r] -> Card (trim s) (trim r) 0 : getCards xs
    [s] -> Card (trim s) "" 0 : getCards xs
    _ -> getCards xs


parseExpr :: [String] -> Either [CDSLExpr] [CDSLParseError]
parseExpr [] = Left []
parseExpr ("":xs) = parseExpr xs
parseExpr (x:xs)
    | ':' `elem` x = case parseIfExpr x of
        Left ex -> case parseExpr xs of
            Left exs -> Left (ex:exs)
            Right errs -> Right errs
        Right err -> case parseExpr xs of
            Right errs -> Right (err ++ errs)
            _ -> Right err
    | otherwise = case parseOneCDSL (words x) 0 of
        Left (ex, ys) -> case parseExpr (unwords ys:xs) of
            Left exs -> Left (ex:exs)
            Right errs -> Right errs
        Right (er, _) -> Right [er]



parseIfExpr :: String -> Either CDSLExpr [CDSLParseError]
parseIfExpr xs = do
    let (y, ys) = (takeWhile (/= ':') xs, removeFirst (dropWhile (/= ':') xs) ':')
    case (parseExpr (stringToList y), parseExpr (stringToList ys)) of
        (Left cond, Left expr) -> Left (If cond expr)
        (Right err, _) -> Right err
        (_, Right err) -> Right err


parseOneCDSL :: [String] -> Int -> Either (CDSLExpr, [String]) (CDSLParseError, Int)
parseOneCDSL [] n = Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Null, rawExpr = "" }, n)
parseOneCDSL (x:xs) n = case x of
    "any" -> case parseOneCDSL xs (n + 1) of
                Left (exr, ys) -> Left (Any exr, ys)
                Right (e, i) -> Right (e { pExpr = Any (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e }, i)
    "all" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (All exr, ys)
        Right (e, i) -> Right (e { pExpr = All (pExpr e), rawExpr = x ++ " " ++ rawExpr e }, i)
    "greatest" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (Greatest exr, ys)
        Right (e, i) -> Right (e { pExpr = Greatest (pExpr e), rawExpr = x ++ " " ++ rawExpr e}, i)
    "players" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (Players exr, ys)
        Right (e, i) -> Right (e { pExpr = Players (pExpr e), rawExpr = x ++ " " ++ rawExpr e }, i)
    "score" -> Left (Score, xs)
    "hand" -> Left (Hand, xs)
    "isEqual" -> case length xs of
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = IsEqual Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs}, n + 1)
        _ -> case (parseOneCDSL [head xs] 0, parseOneCDSL [xs !! 1] 0) of
            (Left (l, _), Left (r, _)) -> Left (IsEqual l r, drop 2 xs)
            (Right (e, i), Left (r, _)) -> Right (e { pExpr = IsEqual (pExpr e) r, rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e }, i)
            (Left (l, _), Right (e, i)) -> Right (e { pExpr = IsEqual l (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e }, i)
            (Right (e, i), _) -> Right (e { pExpr = IsEqual (pExpr e) Null, rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e }, i)
    "isEmpty" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (IsEmpty exr, ys)
        Right (e, i) -> Right (e { pExpr = IsEmpty (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e }, i)
    "swap" -> case length xs of
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Swap Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs}, n + 1)
        _ -> case (parseOneCDSL [head xs] 0, parseOneCDSL [xs !! 1] 0) of
            (Left (l, _), Left (r, _)) -> Left (Swap l r, drop 2 xs)
            (Right (e, i), Left (r, _)) -> Right (e { pExpr = Swap (pExpr e) r }, i)
            (Left (l, _), Right (e, i)) -> Right (e { pExpr = Swap l (pExpr e) }, i)
            (Right l, _) -> Right l
    "shuffle" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (Shuffle exr, ys)
        Right (e, i) -> Right (e { pExpr = Shuffle (pExpr e), rawExpr = if null (rawExpr e) then x else x ++ " " ++ rawExpr e }, i)
    "deck" -> Left (Deck, xs)
    "pile" -> Left (Pile, xs)
    "take" -> case (parseOneCDSL [head xs] 0, parseOneCDSL [xs !! 1] 0, parseOneCDSL [xs !! 2] 0) of
        (Left (i, _), Left (f, _), Left (t, _)) -> Left (Take i f t, drop 3 xs)
        (Right (e, i), _, _) -> Right (e { pErr = IncompleteExpressionError, pExpr = Take Null Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs}, i)
        (_, Right (e, i), _) -> Right (e { pErr = IncompleteExpressionError, pExpr = Take Null Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs}, i)
        (_, _, Right (e, i)) -> Right (e { pErr = IncompleteExpressionError, pExpr = Take Null Null Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs}, i)
    "always" -> Left (Always, xs)
    "never" -> Left (Never, xs)
    "!" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (Not exr, ys)
        Right (e, i) -> Right (e { pErr = IncompleteExpressionError, pExpr = Not Null, rawExpr = if null xs then x else x ++ " " ++ unwords xs}, i)
    "&&" -> case length xs of
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = And Null Null, rawExpr = x}, n + 1)
        _ -> case (parseOneCDSL [head xs] 0, parseOneCDSL [xs !! 1] 0) of
            (Left (l, _), Left (r, _)) -> Left (And l r, drop 2 xs)
            (Right (e, i), Left (r, _)) -> Right (e { pExpr = And (pExpr e) r }, i)
            (Left (l, _), Right (e, i)) -> Right (e { pExpr = And l (pExpr e) }, i)
            (Right (e, i), _) -> Right (e { pExpr = And Null (pExpr e) }, i)
    "||" -> case length xs of
        1 -> Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Or Null Null, rawExpr = x}, n + 1)
        _ -> case (parseOneCDSL [head xs] 0, parseOneCDSL [xs !! 1] 0) of
            (Left (l, _), Left (r, _)) -> Left (Or l r, drop 2 xs)
            (Right (e, i), Left (r, _)) -> Right (e { pExpr = Or (pExpr e) r }, i)
            (Left (l, _), Right (e, i)) -> Right (e { pExpr = Or l (pExpr e) }, i)
            (Right (e, i), _) -> Right (e { pExpr = Or Null (pExpr e) }, i)
    "rank" -> Left (CardRank, xs)
    "suit" -> Left (CardSuit, xs)
    "value" -> Left (CardValue, xs)
    "turnOrder" -> Left (TurnOrder, xs)
    "reset" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (Reset ex, ys)
        Right (e, i) -> Right (e { pExpr = Reset (pExpr e) }, i)
    "player" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (CurrentPlayer ex, ys)
        Right (e, i) -> Right (e { pExpr = CurrentPlayer (pExpr e) }, i)
    "moves" -> Left (PMoves, xs)
    _ -> case readMaybe x :: Maybe Int of
        Just i -> Left (Numeric i, xs)
        _ -> Left (Text x, xs)

validateFeature :: String -> Either Feature CDSLParseError
validateFeature x = case words x of
    -- Card Effects
    ["CARD_EFFECTS"] -> Left CardEffects
    ["change_card"] -> Left CEChangeCard
    ["swap_hand"] -> Left CESwapHand
    ["take_from_hand"] -> Left CETakeFromHand
    ["give_card"] -> Left CEGiveCard
    ["pass_next"] -> Left CEPassNext
    ("draw_card":xs) -> case readMaybe (concat xs) :: Maybe Int of
        Just _ -> Left CEDrawCard
        Nothing -> Right (
            CDSLParseError
            {
                pErr = InvalidFeatureArgumentError
                , pExpr = Null
                , rawExpr = x
            })
    -- Cards
    ["card_suits"] -> Left CardSuits
    ["card_values"] -> Left CardValues
    ["card_ranks"] -> Left CardRanks
    ["card_constraints"] -> Left CardConstraints
    -- Player
    ["player_hand"] -> Left PlayerHand
    ["player_moves"] -> Left PlayerMoves
    -- Time
    ["any_time"] -> Left AnyTime
    ["start_time"] -> Left StartTime
    y -> case fromStringToFeature (unwords y) of
        Just f -> Left f
        Nothing -> Right (
            CDSLParseError
            {
                pErr = NotAFeatureError
                , pExpr = Null
                , rawExpr = unwords y
            })
