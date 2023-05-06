{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module CDSL.ExecCDSLExpr (
    placeCardStmt
    , compareCards
    , execCDSLGame
    , execCDSLBool
    , execCDSLGameBool
    , execCDSLInt
    , execCardEffect
    , fromCDSLToCard
    , fromCDSLToString
    , cardFromCDSL) where

import CardGame.Game (Game (deck, pile, players, cardGen, actions, playerMoves, cardSuits, cardRanks, turnOrder, discard), GameState (TurnEnd))
import Data.CircularList (toList, size, update, fromList, rotR, rotL, focus, isEmpty, rotL, rotNL, rotNR, CList)
import CardGame.Player (Player(hand, pScore, name, moves, movesHistory))
import Data.Either (partitionEithers)
import CardGame.Card (shuffle, Card (..))
import CDSL.CDSLExpr
import Data.List (elemIndex, intercalate)
import Functions (takeUntilDuplicate, deleteAt, removeFirst, updateAt, lookupOrDefault)
import Text.Read (readMaybe, Lexeme (String))
import CardGame.CardFunctions (prettyPrintCards)
import Extra (sleep)
import CardGame.PlayerMove (Move(Pass))
import CDSL.ParseCDSLExpr (toNumeric)
import Data.Maybe (mapMaybe)


placeCardStmt :: [CDSLExpr] -> (Game -> Card -> Bool)
placeCardStmt [] = const . const True
placeCardStmt [Null] = const . const True
placeCardStmt xs
    | any (`elem` xs) [CardRank, CardSuit, CardValue] = compareCards xs
    | otherwise = const . const False


compareCards :: [CDSLExpr] -> Game -> Card -> Bool
compareCards [] _ _ = True
compareCards (x:xs) g c = do
    let pc = fst $ head (pile g)
    let suits = map suit (takeUntilDuplicate (cardGen g))
    let ranks = map rank (takeUntilDuplicate (cardGen g))
    let values = map cScore (takeUntilDuplicate (cardGen g))
    case x of
        CardRank -> elemIndex (rank c) ranks >= elemIndex (rank pc) ranks && compareCards xs g c
        CardSuit -> elemIndex (suit c) suits >= elemIndex (suit pc) suits && compareCards xs g c
        CardValue -> elemIndex (cScore c) values >= elemIndex (cScore pc) values && compareCards xs g c
        _ -> True



execCDSLGame :: [CDSLExpr] -> Game -> IO Game
execCDSLGame [] g = return g
execCDSLGame ((If l r):xs) g = do
    case partitionEithers (map (`execCDSLGameBool` g) l) of
        (res, []) -> if and res
            then
                do
                    g' <- execCDSLGame r g
                    execCDSLGame xs g'
            else
                return g
        _ -> return g
execCDSLGame ((Swap a b):xs) g = if (a == Deck || a == Pile) && (b == Deck || b == Pile)
    then
        case (a, b) of
            (Deck, Pile) -> execCDSLGame xs (g { deck = map fst (pile g), pile = map (\c -> (c, Nothing)) (deck g) })
            (Pile, Deck) -> execCDSLGame xs (g { deck = map fst (pile g), pile = map (\c -> (c, Nothing)) (deck g) })
            _ -> execCDSLGame xs g
    else
        execCDSLGame xs g
execCDSLGame ((Take (Numeric n) a b):xs) g = if (a == Deck || a == Pile) && (b == Deck || b == Pile)
    then
        case (a, b) of
            (Pile, Deck) -> do
                execCDSLGame xs (g { deck =  take n (map fst (pile g)) ++ deck g, pile = drop n (pile g) })
            (Deck, Pile) -> do
                execCDSLGame xs (g { deck =  drop n (deck g) ++ deck g, pile = take n (map (\c -> (c, Nothing)) (deck g)) })
            _ -> execCDSLGame xs g
    else
        execCDSLGame xs g
execCDSLGame ((Shuffle a):xs) g = case a of
    Deck -> do
        deck' <- shuffle (deck g)
        execCDSLGame xs (g { deck = deck' })
    Pile -> do
        pile' <- shuffle (pile g)
        execCDSLGame xs (g { pile = pile' })
    Discard -> do
        discard' <- shuffle (discard g)
        execCDSLGame xs (g { discard = discard' })
    _ -> execCDSLGame xs g
execCDSLGame (Reset (CurrentPlayer PMoves):xs) g = case focus (players g) of
    Just p -> execCDSLGame xs (g { players = update (p { moves = playerMoves g }) (players g) })
    Nothing -> return g
execCDSLGame (AffectPlayer ce ar:xs) g = case ce of
    PassNext -> do
        putStrLn "Your turn was passed."
        sleep 1
        execCDSLGame (turnOrder g ++ xs) g
    DrawCards -> case (focus (players g), ar) of
        (Just p, Just arg) -> do
            let n = sum $ map (\a -> case a of Numeric i -> i; _ -> 0) (mapMaybe toNumeric arg)
            putStrLn ("You must draw " ++ show n ++ " cards")
            sleep 1
            let crds = take n (deck g)
            execCDSLGame xs (g { players = update (p { hand = crds ++ hand p }) (players g) })
        _ -> execCDSLGame (turnOrder g ++ [AffectPlayer ce ar] ++ xs) g
                 
    _ -> execCDSLGame xs g
execCDSLGame (TOLeft:xs) g = execCDSLGame xs (g { players = rotL (players g) })
execCDSLGame (TORight:xs) g = execCDSLGame xs (g { players = rotR (players g) })
execCDSLGame (Put l r:xs) g =
    case (l, r) of
        -- Pile
        (Pile, Deck) -> execCDSLGame xs (g { deck = map fst (pile g) ++ deck g, pile = [] })
        (Pile, Discard) -> execCDSLGame xs (g { discard = map fst (pile g) ++ discard g, pile = [] })
        (Pile, CurrentPlayer Hand) -> case focus (players g) of
            Just p -> execCDSLGame xs (g { pile = [], players = update (p { hand = map fst (pile g) ++ hand p }) (players g) } )
            Nothing -> execCDSLGame xs g

        -- Deck
        (Deck, Pile) -> execCDSLGame xs (g { pile = map (\c -> (c, Nothing)) (deck g) ++ pile g, deck = [] })
        (Deck, Discard) -> execCDSLGame xs (g { discard = deck g ++ discard g, deck = [] })
        (Deck, CurrentPlayer Hand) -> case focus (players g) of
            Just p -> execCDSLGame xs (g { deck = [], players = update (p { hand = deck g ++ hand p }) (players g) } )
            Nothing -> execCDSLGame xs g
        -- Discard
        (Discard, Pile) -> execCDSLGame xs (g { pile = map (\c -> (c, Nothing)) (discard g) ++ pile g, discard = [] })
        (Discard, Deck) -> execCDSLGame xs (g { deck = discard g ++ deck g, discard = [] })
        (Discard, PreviousPlayer Hand) -> do
            g' <- execCDSLGame (turnOrder g) g
            case focus (players g') of
                Just p -> execCDSLGame (GoForward Turn:xs) (g' { discard = [], players = update (p { hand = discard g' ++ hand p }) (players g') } )
                Nothing -> execCDSLGame xs g
        _ -> execCDSLGame xs g
execCDSLGame (GoBack Turn:xs) g = case turnOrder g of
    [TOLeft] -> execCDSLGame (TORight:xs) g
    [TORight] -> execCDSLGame (TOLeft:xs) g
    _ -> execCDSLGame xs g
execCDSLGame (GoForward Turn:xs) g = case turnOrder g of
    [TOLeft] -> execCDSLGame (TOLeft:xs) g
    [TORight] -> execCDSLGame (TORight:xs) g
    _ -> execCDSLGame xs g
execCDSLGame (_:xs) g = execCDSLGame xs g



execCDSLBool :: CDSLExpr -> Either Bool CDSLExecError
execCDSLBool Always = Left True
execCDSLBool Never = Left False
execCDSLBool (IsEqual (Numeric a) (Numeric b)) = Left (a == b)
execCDSLBool (Not ex) = case partitionEithers $ map execCDSLBool ex of
    (b, []) -> Left (not $ and b)
    (_, e) -> Right $ head e
execCDSLBool (Or l r) = case (partitionEithers $ map execCDSLBool l, partitionEithers $ map execCDSLBool r) of
    ((lt, []), (rt, [])) -> Left (or lt || or rt)
    ((_, e), _) -> Right $ head e
execCDSLBool (And l r) = case (partitionEithers $ map execCDSLBool l, partitionEithers $ map execCDSLBool r) of
    ((lt, []), (rt, [])) -> Left (and lt && and rt)
    ((_, e), _) -> Right $ head e
execCDSLBool e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })


execCDSLGameBool :: CDSLExpr -> Game -> Either Bool CDSLExecError
execCDSLGameBool (IsEmpty Deck) g = Left (null (deck g))
execCDSLGameBool (IsEmpty Pile) g = Left (null (pile g))
execCDSLGameBool e@(IsEqual a b) g = case (fromCDSLToCard a, fromCDSLToCard b) of
    (Just (Left fa), Just (Right fb)) -> Left (fa g == map fst (fb g))
    (Just (Right fb), Just (Left fa)) -> Left (fa g == map fst (fb g))
    (Nothing, _) -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = e })
    (_, Nothing) -> Right (CDSLExecError { err = SyntaxErrorRightOperand, expr = e })
    _ -> Right (CDSLExecError { err = SyntaxErrorLeftOperand, expr = e })
execCDSLGameBool (Any (Players (IsEmpty Hand))) g = Left (any (null . hand) (toList (players g)))
execCDSLGameBool (All (Players (IsEmpty Hand))) g = Left (all (null . hand) (toList (players g)))
execCDSLGameBool e@(Any (Players (IsEqual a b))) g = case ((a == Score, b == Score), (execCDSLInt a, execCDSLInt b)) of
    ((True, _), (_, Left n)) -> Left (any ((==n) . pScore) (toList (players g)))
    ((_, True), (Left n, _)) -> Left (any ((==n) . pScore) (toList (players g)))
    ((False, _), (_, Right _)) -> Right (CDSLExecError { err = InvalidSyntaxError, expr = a })
    ((_, False), (Right _, _)) -> Right (CDSLExecError { err = InvalidSyntaxError, expr = b })
    _ -> Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
execCDSLGameBool Always _ = Left True
execCDSLGameBool Never _ = Left False
execCDSLGameBool (IsSame cf (Look (Numeric n) Pile)) g = if length (pile g) < n
    then
        Left False
    else
        Left (hasSameField (take n (map fst $ pile g)) [cf])
execCDSLGameBool (CurrentPlayer (IsMove PAPass)) g = case focus (players g) of
    Just p -> if null (movesHistory p)
        then
            Left False
        else
            case head (movesHistory p) of
                (Pass, _) -> Left True
                _ -> Left False
    Nothing -> Left False
execCDSLGameBool e@(PreviousPlayer (IsMove PAPass)) g = case turnOrder g of
    [TOLeft] -> case focus (rotR (players g)) of
        Just p -> if null (movesHistory p)
            then
                Left False
            else
                case head (movesHistory p) of
                    (Pass, _) -> Left True
                    _ -> Left False
        Nothing -> Left False
    [TORight] -> case focus (rotL (players g)) of
        Just p -> if null (movesHistory p)
            then
                Left False
            else
                case head (movesHistory p) of
                    (Pass, _) -> Left True
                    _ -> Left False
        Nothing -> Left False
    _ -> Right (CDSLExecError { err = InvalidSyntaxError, expr = e })
execCDSLGameBool e _ = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })


-- Checks if the givens all have equal card fields
hasSameField :: [Card] -> [CDSLExpr] -> Bool
hasSameField [] _ = True
hasSameField _ [] = True
hasSameField cs@(c:_) (x:xs) = case x of
    CardSuit -> all (\c' -> suit c' == suit c) cs
    CardRank -> all (\c' -> rank c' == rank c) cs
    CardValue -> all (\c' -> cScore c' == cScore c) cs
    _ -> hasSameField cs xs


execCDSLInt :: CDSLExpr -> Either Int CDSLExecError
execCDSLInt (Numeric n) = Left n
execCDSLInt e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })

fromCDSLToCard :: CDSLExpr -> Maybe (Either (Game -> [Card]) (Game -> [(Card, Maybe Card)]))
fromCDSLToCard x = case x of
    Pile -> Just (Right pile)
    Deck -> Just (Left deck)
    _ -> Nothing


execCardEffect :: (CardEffect, Maybe [CDSLExpr]) -> Game -> Player -> IO Game
execCardEffect (ce, a) g plr = case (ce, a) of
    (ChangeCard, Just arg) -> do
        c <- createCard arg g
        let pc = fst $ head (pile g)
        return (g { pile = (pc, Just c) : drop 1 (pile g) })

    (GiveCard, _) -> do
        (p, i) <- choosePlayer g plr
        (p', c) <- chooseCard plr
        let plrs = rotatePlr g i (update (p { hand = c:hand p }) (players g)  )
        return (g { players = rotatePlrBack g i (update p' plrs) })

    (SwapHand, _) -> do
        (p, i) <- choosePlayer g plr
        let plrs = rotatePlr g i (update (plr { hand = hand p }) (players g))
        return (g { players = rotatePlrBack g i (update (p { hand = hand plr }) plrs) })

    (TakeFromHand, _) -> do
        (p, i) <- choosePlayer g plr
        (p', c) <- chooseCard p
        let plrs = rotatePlr g i (update (p { hand = c:hand p }) (players g)  )
        return (g { players = rotatePlrBack g i (update p' plrs) })

    (PassNext, _) -> return (g { actions = (TurnEnd, [(execCDSLGame [AffectPlayer PassNext Nothing], False)]) : actions g})

    (DrawCards, arg) -> return (g { actions = (TurnEnd, [(execCDSLGame [AffectPlayer DrawCards arg], False)]) : actions g})

    _ -> return g


rotatePlr :: Game -> Int -> CList Player -> CList Player
rotatePlr g i plr = case turnOrder g of
    [TOLeft] -> rotNL i plr
    [TORight] -> rotNR i plr
    _ -> plr

rotatePlrBack :: Game -> Int -> CList Player -> CList Player
rotatePlrBack g i plr = case turnOrder g of
    [TOLeft] -> rotNR i plr
    [TORight] -> rotNL i plr
    _ -> plr

choosePlayer :: Game -> Player -> IO (Player, Int)
choosePlayer g p = do
    putStrLn (intercalate "," (map name (removeFirst (toList (players g)) p)) )
    putStrLn ("Choose player (1/" ++ show (size (players g) - 1) ++ ")")
    c <- getLine
    case readMaybe c :: Maybe Int of
        Just i -> return (removeFirst (toList (players g)) p !! (i - 2), i - 2)
        Nothing -> do
            putStrLn "Invalid choice"
            choosePlayer g p

chooseCard :: Player -> IO (Player, Card)
chooseCard p = do
    prettyPrintCards (hand p)
    putStrLn ("Choose Card (1/" ++ show (length (hand p)) ++ ")")
    c <- getLine
    case readMaybe c :: Maybe Int of
        Just i -> return ((p { hand = deleteAt (i - 1) (hand p) }), hand p !! (i - 1))
        Nothing -> do
            putStrLn "Invalid choice"
            chooseCard p

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
fromCDSLToString (Not e) = "!" ++ intercalate ", " (map fromCDSLToString e)
fromCDSLToString (And l r) = "&& " ++ intercalate ", " (map fromCDSLToString l) ++ " " ++ intercalate ", " (map fromCDSLToString r)
fromCDSLToString (Or l r) = "|| " ++ intercalate ", " (map fromCDSLToString l) ++ " " ++ intercalate ", " (map fromCDSLToString r)
fromCDSLToString CardRank = "ranks"
fromCDSLToString CardSuit = "suits"
fromCDSLToString CardValue = "value"
fromCDSLToString (Text s) = s
fromCDSLToString (PlayerAction a b) = show a ++ " " ++ if b then "TRUE" else "FALSE"
fromCDSLToString (AffectPlayer ce (Just ar)) = "affect_player " ++ show ar ++ show ce 
fromCDSLToString (AffectPlayer ce Nothing) = "affect_player " ++ show ce
fromCDSLToString (CEffect ce cs) = "card_effect " ++ show ce ++ " " ++ show cs
fromCDSLToString (Reset ce) = "reset " ++ fromCDSLToString ce
fromCDSLToString (CurrentPlayer ce) = "player " ++ fromCDSLToString ce
fromCDSLToString PMoves = "moves"
fromCDSLToString (Cards ce) = show ce
fromCDSLToString CLe = "le"
fromCDSLToString CGr = "ge"
fromCDSLToString CGRq = "gte"
fromCDSLToString CLEq = "lte"
fromCDSLToString CEq = "eq"
fromCDSLToString TOLeft = "left"
fromCDSLToString TORight = "right"
fromCDSLToString Discard = "discard"
fromCDSLToString (IsSame l r) = "isSame " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString (Look l r) = "look " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString (Put l r) = "put " ++ fromCDSLToString l ++ " " ++ fromCDSLToString r
fromCDSLToString (IsMove ex) = "isMove " ++ fromCDSLToString ex
fromCDSLToString PAPass = "pPass"
fromCDSLToString PADraw = "pDraw"
fromCDSLToString PAPlay = "pPlay"
fromCDSLToString (PreviousPlayer ex) = "prevPlayer " ++ fromCDSLToString ex
fromCDSLToString (GoBack ex) = "goBack " ++ fromCDSLToString ex
fromCDSLToString (GoForward ex) = "goForward " ++ fromCDSLToString ex
fromCDSLToString Turn = "turn"
fromCDSLToString _ = "(NOT ADDED)"

createCard :: [CDSLExpr] -> Game -> IO Card
createCard = cc []
    where
        cc :: [(CDSLExpr, String)] -> [CDSLExpr] -> Game -> IO Card
        cc xs [] _ = case (lookupOrDefault CardSuit "" xs, lookupOrDefault CardRank "" xs, lookupOrDefault CardValue "0" xs) of
            (cs, cr, cv) -> case readMaybe cv :: Maybe Int of
                Just i -> return (Card cs cr i)
                Nothing -> return (Card cs cr 0)
        cc xs (y:ys) g = case y of
            CardSuit -> do
                putStrLn (intercalate "," (map fromCDSLToString (cardSuits g)))
                putStrLn ("Choose Card Suits (1/" ++ show (length (cardSuits g)) ++ ")")
                c <- getLine
                case readMaybe c :: Maybe Int of
                    Just i -> if i >= 1 && i <= length (cardSuits g)
                        then
                            do
                                putStrLn ("Choose: " ++ show (cardSuits g !! (i - 1)))
                                cc ((CardSuit, fromCDSLToString (cardSuits g !! (i - 1))):xs) ys g
                        else
                            do
                                putStrLn ("Expected a number between 1 and " ++ show (length (cardSuits g)))
                                cc xs (y:ys) g
                    Nothing -> do
                        putStrLn ("Expected a number between 1 and " ++ show (length (cardSuits g)))
                        cc xs (y:ys) g
            CardRank -> do
                putStrLn (intercalate "," (map fromCDSLToString (cardRanks g)))
                putStrLn ("Choose Card Ranks (1/" ++ show (length (cardRanks g)) ++ ")")
                c <- getLine
                case readMaybe c :: Maybe Int of
                    Just i -> if i >= 1 && i <= length (cardRanks g)
                        then
                            do
                                putStrLn ("Choose: " ++ fromCDSLToString (cardRanks g !! (i - 1)))
                                cc ((CardRank, fromCDSLToString (cardRanks g !! (i - 1))):xs) ys g
                        else
                            do
                                putStrLn ("Expected a number between 1 and " ++ show (length (cardRanks g)))
                                cc xs (y:ys) g
                    Nothing -> do
                        putStrLn ("Expected a number between 1 and " ++ show (length (cardRanks g)))
                        cc xs (y:ys) g
            _ -> error (show y)

cardFromCDSL :: [CDSLExpr] -> [Card]
cardFromCDSL [] = []
cardFromCDSL ((Cards cs):xs) = cs ++ cardFromCDSL xs
cardFromCDSL (_:xs) = cardFromCDSL xs