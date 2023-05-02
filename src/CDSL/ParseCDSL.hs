module CDSL.ParseCDSL (
    isCDSLExprNumeric
    , parseCDSLPlayerAction
    , parseStringList
    , readCDSL
    , getCards
    , validateFeature
    , processIfString
    , toNumeric
    , parseExpr
    , parseOneCDSL
) where

import Text.Read (readMaybe)
import CDSL.CDSLExpr
import Data.List.Extra (splitOn, trim)
import Data.Text (unpack, strip, pack)
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass))
import Feature
import CardGame.Player (parsePlayerMove)
import Data.Maybe (mapMaybe)
import CardGame.Card
import Functions (subString, stringToList, removeFirst)



isCDSLExprNumeric :: CDSLExpr -> Bool
isCDSLExprNumeric (Numeric _) = True
isCDSLExprNumeric Score = True
isCDSLExprNumeric _ = False



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


getCardFields :: [String] -> Either [CDSLExpr] [CDSLParseError]
getCardFields [] = Left []
getCardFields (x:xs) = case x of
    "rank" -> case getCardFields xs of
        Left ex -> Left (CardRank:ex)
        e -> e
    "suits" -> case getCardFields xs of
        Left ex -> Left (CardSuit:ex)
        e -> e
    "value" -> case getCardFields xs of
        Left ex -> Left (CardValue:ex)
        e -> e
    _ -> case getCardFields xs of
        Right err -> Right (CDSLParseError { pErr = NotACardFieldError, pExpr = Null, rawExpr = x }:err)
        _ -> Right [CDSLParseError { pErr = NotACardFieldError, pExpr = Null, rawExpr = x }]

readCDSL :: String -> Either ((Feature, Maybe [CDSLExpr]), [CDSLExpr]) (Maybe Feature, [CDSLParseError])
readCDSL xs = do
    let (y, ys) = (takeWhile (/='=') xs, removeFirst (dropWhile (/='=') xs) '=')
    case validateFeature (unpack $ strip $ pack y) of
        -- Cards
        Left CEDrawCard -> case readMaybe (unwords (drop 1 (words y))) :: Maybe Int of
            Just i -> Left ((CEDrawCard, Just [Numeric i]), [CEffect (DrawCards i) (getCards (stringToList ys))])
            Nothing -> Right (Just CardEffects, [CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = show (drop 1 (words y)) }])
        Left CESwapHand -> Left ((CESwapHand, Nothing), [CEffect SwapHand (getCards (stringToList ys))])
        Left CEChangeCard -> case getCardFields (drop 1 (words y)) of
            Left ex -> Left ((CEChangeCard, Nothing), [CEffect (ChangeCard ex) (getCards (stringToList ys))])
            Right err -> Right (Just CEChangeCard, err)
        Left CETakeFromHand -> Left ((CETakeFromHand, Nothing), [CEffect TakeFromHand (getCards (stringToList ys))])
        Left CEPassNext -> Left ((CEPassNext, Nothing), [CEffect PassNext (getCards (stringToList ys))])
        Left CEGiveCard -> Left ((CEGiveCard, Nothing), [CEffect GiveCard (getCards (stringToList ys))])
        Left PlayerMoves -> Left ((PlayerMoves, Nothing), map (uncurry PlayerAction) (mapMaybe parsePlayerMove (stringToList ys)))
        Left ExceptionConstraints -> case getCardComperator (unwords (drop 1 (words y))) of
            Just cc -> case parseExpr (stringToList ys) of
                Left expr -> Left ((ExceptionConstraints, Just [cc]), expr)
                Right err -> Right (Just ExceptionConstraints, err)
            _ -> Right (Just ExceptionConstraints, [CDSLParseError {rawExpr=xs, pExpr=Null, pErr=InvalidFeatureArgumentError}])
        Left IgnoreConstraints -> Left ((IgnoreConstraints, Nothing), [Cards (getCards (stringToList ys))])
        Left f -> case parseExpr (stringToList ys) of
            Left exprs -> Left ((f, Nothing), exprs)
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
parseExpr (('!':xs):ys) = case parseExpr [xs] of
    Left ex -> case parseExpr ys of
        Left exprs -> Left (Not ex:exprs)
        e -> e
    e -> e
parseExpr (x:xs)
    | "||" `subString` x = case (takeWhile (/= '|') x, drop 2 $ dropWhile (/= '|') x) of
        (l, r) -> case (parseExpr [l], parseExpr [r]) of
            (Left l', Left r') -> case parseExpr xs of
                Left ex -> Left (Or l' r':ex)
                Right err -> Right err
            (Right le, Right re) -> Right (le ++ re)
            (Right err, _) -> Right err
            (_, Right err) -> Right err
    | "&&" `subString` x = case (takeWhile (/= '|') x, drop 2 $ dropWhile (/= '|') x) of
        (l, r) -> case (parseExpr [l], parseExpr [r]) of
            (Left l', Left r') -> case parseExpr xs of
                Left ex -> Left (Or l' r':ex)
                Right err -> Right err
            (Right le, Right re) -> Right (le ++ re)
            (Right err, _) -> Right err
            (_, Right err) -> Right err
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
    "prevPlayer" -> case parseOneCDSL xs (n + 1) of
        Left (exr, ys) -> Left (PreviousPlayer exr, ys)
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
    "rank" -> Left (CardRank, xs)
    "suit" -> Left (CardSuit, xs)
    "value" -> Left (CardValue, xs)
    "discard" -> Left (Discard, xs)
    "left" -> Left (TOLeft, xs)
    "right" -> Left (TORight, xs)
    "turn" -> Left (Turn, xs)
    "goBack" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (GoBack ex, ys)
        e -> e
    "goForward" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (GoForward ex, ys)
        e -> e
    "reset" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (Reset ex, ys)
        Right (e, i) -> Right (e { pExpr = Reset (pExpr e) }, i)
    "player" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (CurrentPlayer ex, ys)
        Right (e, i) -> Right (e { pExpr = CurrentPlayer (pExpr e) }, i)
    "moves" -> Left (PMoves, xs)
    "le" -> Left (CLe, xs)
    "ge" -> Left (CGr, xs)
    "lte" -> Left (CLEq, xs)
    "gte" -> Left (CGRq, xs)
    "eq" -> Left (CEq, xs)
    "isMove" -> case parseOneCDSL xs (n + 1) of
        Left (ex, ys) -> Left (IsMove ex, ys)
        Right (e, i) -> Right (e { pExpr = IsMove (pExpr e) }, i)
    "pPass" -> Left (PAPass, xs)
    "pDraw" -> Left (PADraw, xs)
    "pPlay" -> Left (PAPlay, xs)
    "isSame" -> case parseOneCDSL xs (n + 1) of
        Left (l, ys) -> case parseOneCDSL ys (n + 1) of
            Left (r, zs) -> Left (IsSame l r, zs)
            e -> e
        e -> e
    "look" -> case parseOneCDSL xs (n + 1) of
        Left (l, ys) -> case parseOneCDSL ys (n + 1) of
            Left (r, zs) -> Left (Look l r, zs)
            e -> e
        e -> e
    "put" -> case parseOneCDSL xs (n + 1) of
        Left (l, ys) -> case parseOneCDSL ys (n + 1) of
            Left (r, zs) -> Left (Put l r, zs)
            e -> e
        e -> e
    "null" -> Left (Null, xs)
    _ -> case readMaybe x :: Maybe Int of
        Just i -> Left (Numeric i, xs)
        _ -> Left (Text x, xs)

validateFeature :: String -> Either Feature CDSLParseError
validateFeature x = case words x of
    -- Card Effects
    ["CARD_EFFECTS"] -> Left CardEffects
    ("change_card":xs) -> case getCardFields xs of
        Left _ -> Left CEChangeCard
        Right e -> Right (head e)
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
    ["ignore_constraints"] -> Left IgnoreConstraints
    -- Player
    ["player_hand"] -> Left PlayerHand
    ["player_moves"] -> Left PlayerMoves
    -- Time
    ["any_time"] -> Left AnyTime
    ["start_time"] -> Left StartTime
    ("exception_constraints":xs) -> case getCardComperator (concat xs) of
        Just _ -> Left ExceptionConstraints
        Nothing -> Right (
            CDSLParseError
            {
                pErr = InvalidFeatureArgumentError
                , pExpr = Null
                , rawExpr = x
            })
    y -> case fromStringToFeature (unwords y) of
        Just f -> Left f
        Nothing -> Right (
            CDSLParseError
            {
                pErr = NotAFeatureError
                , pExpr = Null
                , rawExpr = unwords y
            })


getCardComperator :: String -> Maybe CDSLExpr
getCardComperator "eq" = Just CEq
getCardComperator "lte" = Just CLEq
getCardComperator "gte" = Just CGRq
getCardComperator "le" = Just CLe
getCardComperator "ge" = Just CGr
getCardComperator _ = Nothing