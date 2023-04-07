module CDSL.ExecCDSLExpr where

import CardGame.Game (Game (deck, pile, players, cardGen))
import Data.CircularList (toList)
import CardGame.Player (Player(hand, pScore))
import Data.Either (partitionEithers)
import CardGame.Card (shuffle, Card (suit, rank, cScore))
import CDSL.CDSLExpr
import Data.List (elemIndex)
import Functions (takeUntilDuplicate)


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





execCDSLGame :: [CDSLExpr] -> Game -> Game
execCDSLGame [] g = g
execCDSLGame ((If l r):xs) g = case partitionEithers (map execCDSLBool l) of
    (res, []) -> if all (== True) res
        then
            execCDSLGame xs (execCDSLGame r g)
        else
            g
    _ -> g
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