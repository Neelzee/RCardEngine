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

import CardGame.Game (Game (deck, pile, players, cardGen, actions, playerMoves, cardSuits, cardRanks), GameState (TurnEnd))
import Data.CircularList (toList, size, update, fromList, rotNR, rotNL, focus, rotR, isEmpty)
import CardGame.Player (Player(hand, pScore, name, moves))
import Data.Either (partitionEithers)
import CardGame.Card (shuffle, Card (..))
import CDSL.CDSLExpr
import Data.List (elemIndex, intercalate)
import Functions (takeUntilDuplicate, deleteAt, removeFirst, updateAt, lookupOrDefault)
import Text.Read (readMaybe, Lexeme (String))
import CardGame.CardFunctions (prettyPrintCards)
import Extra (sleep)


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
execCDSLGame ((If l r):xs) g = case partitionEithers (map (`execCDSLGameBool` g) l) of
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
    Deck -> execCDSLGame xs (g { deck = shuffle (deck g) })
    Pile -> execCDSLGame xs (g { pile = shuffle (pile g) })
    _ -> execCDSLGame xs g
execCDSLGame (Reset (CurrentPlayer PMoves):xs) g = case focus (players g) of
    Just p -> execCDSLGame xs (g { players = update (p { moves = playerMoves g }) (players g) })
    Nothing -> return g
execCDSLGame (AffectPlayer ce:xs) g = case ce of
    PassNext -> do
        putStrLn "Your turn was passed."
        sleep 1
        execCDSLGame xs (g { players = rotR (players g) })
    (DrawCards n) -> case focus (players g) of
        Just p -> do
            putStrLn ("You must draw " ++ show n ++ " cards")
            sleep 1
            let crds = take n (deck g)
            execCDSLGame xs (g { players = update (p { hand = crds ++ hand p }) (players g) })
        Nothing -> if isEmpty (players g)
            then
                return g
            else
                 execCDSLGame (AffectPlayer ce:xs) (g { players = rotR (players g) })
    _ -> execCDSLGame xs g
execCDSLGame (_:xs) g = execCDSLGame xs g



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
execCDSLGameBool e _ = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })


execCDSLInt :: CDSLExpr -> Either Int CDSLExecError
execCDSLInt (Numeric n) = Left n
execCDSLInt e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })

fromCDSLToCard :: CDSLExpr -> Maybe (Either (Game -> [Card]) (Game -> [(Card, Maybe Card)]))
fromCDSLToCard x = case x of
    Pile -> Just (Right pile)
    Deck -> Just (Left deck)
    _ -> Nothing


execCardEffect :: CardEffect -> Game -> Player -> IO Game
execCardEffect ce g plr = case ce of
    ChangeCard xs -> do
        c <- createCard xs g
        let pc = fst $ head (pile g) 
        return (g { pile = (pc, Just c) : drop 1 (pile g) })

    GiveCard -> do
        (p, i) <- choosePlayer g plr
        (p', c) <- chooseCard plr
        let plrs = rotNR i (update (p { hand = c:hand p }) (players g)  )
        return (g { players = rotNL i (update p' plrs) })

    SwapHand -> do
        (p, i) <- choosePlayer g plr
        let plrs = rotNR i (update (plr { hand = hand p }) (players g))
        return (g { players = rotNL i (update (p { hand = hand plr }) plrs) })

    TakeFromHand -> do
        (p, i) <- choosePlayer g plr
        (p', c) <- chooseCard p
        let plrs = rotNR i (update (p { hand = c:hand p }) (players g)  )
        return (g { players = rotNL i (update p' plrs) })

    PassNext -> return (g { actions = (TurnEnd, [(execCDSLGame [AffectPlayer PassNext], False)]) : actions g})

    (DrawCards n) -> return (g { actions = (TurnEnd, [(execCDSLGame [AffectPlayer (DrawCards n)], False)]) : actions g})

    _ -> return g



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
fromCDSLToString (Cards ce) = show ce
fromCDSLToString CLe = "le"
fromCDSLToString CGr = "ge"
fromCDSLToString CGRq = "gte"
fromCDSLToString CLEq = "lte"
fromCDSLToString CEq = "eq"
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
            _ -> undefined

cardFromCDSL :: [CDSLExpr] -> [Card]
cardFromCDSL [] = []
cardFromCDSL ((Cards cs):xs) = cs ++ cardFromCDSL xs
cardFromCDSL (_:xs) = cardFromCDSL xs