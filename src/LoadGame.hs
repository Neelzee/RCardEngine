module LoadGame where
import CardGame.Game
import GameData.GD (GameData)
import GameData.LoadGD (loadGameData)
import CardGame.Card (Card (Card, cScore, suit), rank)
import Data.List.Extra (splitOn, trim, sortBy)
import Feature (Feature(CardSuits, CardRanks, CardValues, PlayerMoves, PileCount, PlayerHand, EndCon, WinCon, CardConstraints, AnyTime, StartTime, GameName, CardEffects, TurnStartTime, TurnEndTime, IgnoreConstraints, CardCompare, ExceptionConstraints, TurnOrder))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, mapMaybe)
import CardGame.Player (standardMoves, resetMoves, parsePlayerMovesExpr, Player (pScore, hand))
import CDSL.CDSLExpr (CDSLExpr(Numeric, If, Greatest, Players, Score, IsEqual, All, IsEmpty, Hand, CardValue, CardRank, CardSuit, Text, CLe, CGr, CLEq, CGRq, Null))
import CDSL.ExecCDSLExpr (execCDSLGame, execCDSLBool, execCDSLGameBool, cardFromCDSL)
import Data.CircularList (toList, fromList)
import Functions (lookupAll, lookupOrDefault)
import CDSL.ParseCardDSL (toNumeric)
import CardGame.CardFunctions (defaultCardSuits, defaultCardRanks, defaultCardValues, makeDeck, cardElem)

loadGame :: Game -> Int -> IO Game
loadGame g n = do
    res <- loadGameData [] n
    case res of
        Left gd -> loadGame' gd g
        Right e -> do
            print e
            return g





loadGame' :: GameData -> Game -> IO Game
loadGame' rls g = do
    -- gamename
    gm <- case lookup GameName rls of
        Just [Text nm] -> return nm
        _ -> return "game"
    cs <- case lookup CardSuits rls of
        Just suits -> return suits
        _ -> return defaultCardSuits
    cr <- case lookup CardRanks rls of
        Just ranks -> return ranks
        _ -> return defaultCardRanks
    cv <- case lookup CardValues rls of
        Just values -> return (mapMaybe toNumeric values)
        _ -> return defaultCardValues
    let ccI = getComparatorInt (lookupOrDefault CardCompare [] rls)
    let ccS = getComparatorString (lookupOrDefault CardCompare [] rls)
    let cards' = makeDeck cs cr cv


    let ce = concat (lookupAll CardEffects rls)

    let to = lookupAll TurnOrder rls

    let cg = cycle cards'
    -- Player
    mv <- case lookup PlayerMoves rls of
        Just mvs -> return (parsePlayerMovesExpr mvs)
        Nothing -> return standardMoves
    -- Pile
    let pl = case lookup PileCount rls of
            Just e -> e
            _ -> [Numeric 1]

    -- Card Count
    let hc = lookupOrDefault PlayerHand [Numeric 3] rls

    -- End con
    let ec = map createEndCon (lookupAll EndCon rls)

    -- Win con
    let wc = map createWinCon (lookupAll WinCon rls)

    cex <- case lookupOrDefault ExceptionConstraints [] rls of
        [cc, CardSuit] -> return (Right (getComparatorString [cc]), CardSuit)
        [cc, CardRank] -> return (Right (getComparatorString [cc]), CardRank)
        [cc, CardValue] -> return (Left (getComparatorInt [cc]), CardValue)
        _ -> return (Left (\_ _ -> False), Null)

    -- Can place cards
    let pc = placeCardStmt (concat (lookupAll CardConstraints rls)) ccI ccS cex
    -- Rules that should be checked at specific times
    -- Anytime
    let at = map execCDSLGame (lookupAll AnyTime rls)

    -- Start
    let st = map execCDSLGame (lookupAll StartTime rls)

    -- TurnStart
    let ts = map execCDSLGame (lookupAll TurnStartTime rls)
    let te = map execCDSLGame (lookupAll TurnEndTime rls)



    return g {
        deck = cards'
        , cardSuits = cs
        , cardRanks = cr
        , turnOrder = concat to
        , cardEffects = ce
        , gameName = gm
        , cardGen = cg
        , playerMoves = mv
        , players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
        , actions = [
            (Start, map (\val -> (val, True)) at)
            , (Start, map (\val -> (val, True)) st)
            , (TurnStart, map (\val -> (val, True)) at)
            , (TurnEnd, map (\val -> (val, True)) at)
            , (TurnStart, map (\val -> (val, True)) ts)
            , (TurnEnd, map (\val -> (val, True)) te)
            ]
        , rules = [(PlayerHand, hc),(PileCount, pl)]
        , canPlaceCard = [pc]
    }
    where
        getComparatorInt :: [CDSLExpr] -> (Int -> Int -> Bool)
        getComparatorInt [] = (==)
        getComparatorInt (x:_) = case x of
            CLe -> (>)
            CGr -> (<)
            CLEq -> (>=)
            CGRq -> (<=)
            _ -> (==)

        getComparatorString :: [CDSLExpr] -> (String -> String -> Bool)
        getComparatorString [] = (==)
        getComparatorString (x:_) = case x of
            CLe -> (>)
            CGr -> (<)
            CLEq -> (>=)
            CGRq -> (<=)
            _ -> (==)

        placeCardStmt :: [CDSLExpr] -> (Int -> Int -> Bool) -> (String -> String -> Bool) -> (Either (Int -> Int -> Bool) (String -> String -> Bool), CDSLExpr) -> Game -> Card -> Bool
        placeCardStmt [] _ _ _ _ _ = True
        placeCardStmt (x:xs) fi fs ex gm c@(Card s r v)
            | null (pile gm) = True
            | c `cardElem` cardFromCDSL (lookupOrDefault IgnoreConstraints [] rls) = True
            | uncurry isException ex = True
            | otherwise = do
                    let (Card s' r' v') = fst $ head (pile gm)
                    -- Checks :)
                    case x of
                        CardValue -> if v' == 0
                        then
                            placeCardStmt xs fi fs ex gm c
                        else
                            v `fi` v' && placeCardStmt xs fi fs ex gm c
                        CardRank -> if r' == ""
                            then
                                placeCardStmt xs fi fs ex gm c
                            else
                                r `fs` r' && placeCardStmt xs fi fs ex gm c
                        CardSuit -> if s' == ""
                            then
                                placeCardStmt xs fi fs ex gm c
                            else
                                s `fs` s' && placeCardStmt xs fi fs ex gm c
                        _ -> placeCardStmt xs fi fs ex gm c
            where
                isException :: Either (Int -> Int -> Bool) (String -> String -> Bool) -> CDSLExpr -> Bool
                isException (Left f') CardValue = v `f'` cScore (fst $ head (pile gm))
                isException (Right f') CardSuit = s `f'` suit (fst $ head (pile gm))
                isException (Right f') CardRank = r `f'` rank (fst $ head (pile gm))
                isException _ _ = False


createEndCon :: [CDSLExpr] -> Game -> Bool
createEndCon [] _ = False
createEndCon (x:xs) g = case execCDSLGameBool x g of
    Left b -> b || createEndCon xs g
    Right _ -> createEndCon xs g

createWinCon :: [CDSLExpr] -> Game -> [Player]
createWinCon [] _ = []
createWinCon (x:xs) g = case x of
    (Greatest (Players Score)) -> sortBy (\p1 p2 -> compare (pScore p1) (pScore p2)) (toList (players g))
    (All (Players (IsEqual Score (Numeric n)))) -> filter ((== n) . pScore) (toList (players g)) ++ createWinCon xs g
    (All (Players (IsEmpty Hand))) -> filter (null . hand) (toList (players g)) ++ createWinCon xs g
    _ -> []


