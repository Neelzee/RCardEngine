module CDSL.ExecCDSLExpr where

import CardGame.Game (Game (deck, pile, players, cardGen, actions, playerMoves), GameState (TurnEnd))
import Data.CircularList (toList, size, update, fromList, rotNR, rotNL, focus, rotR, isEmpty)
import CardGame.Player (Player(hand, pScore, name, moves))
import Data.Either (partitionEithers)
import CardGame.Card (shuffle, Card (suit, rank, cScore), CardEffect (..))
import CDSL.CDSLExpr
import Data.List (elemIndex, intercalate)
import Functions (takeUntilDuplicate, deleteAt, removeFirst, updateAt)
import Text.Read (readMaybe)
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
    let pc = head (pile g)
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
execCDSLGame ((If l r):xs) g = case partitionEithers (map execCDSLBool l) of
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
            (Deck, Pile) -> execCDSLGame xs (g { deck = pile g, pile = deck g })
            (Pile, Deck) -> execCDSLGame xs (g { deck = pile g, pile = deck g })
            _ -> execCDSLGame xs g
    else
        execCDSLGame xs g
execCDSLGame ((Take (Numeric n) a b):xs) g = if (a == Deck || a == Pile) && (b == Deck || b == Pile)
    then
        case (a, b) of
            (Pile, Deck) -> execCDSLGame xs (g { deck =  take n (pile g) ++ deck g, pile = drop n (pile g) })
            (Deck, Pile)->  execCDSLGame xs (g { deck =  drop n (pile g) ++ deck g, pile = take n (pile g) })
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


execCDSLInt :: CDSLExpr -> Either Int CDSLExecError
execCDSLInt (Numeric n) = Left n
execCDSLInt e = Right (CDSLExecError { err = InvalidSyntaxError, expr = e })

fromCDSLToCard :: CDSLExpr -> Maybe (Game -> [Card])
fromCDSLToCard x = case x of
    Pile -> Just pile
    Deck -> Just deck
    _ -> Nothing


execCardEffect :: CardEffect -> Game -> Player -> IO Game
execCardEffect ce g plr = case ce of
    ChangeCard -> return g

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