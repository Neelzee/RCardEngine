module LoadGame (loadGame) where
import CardGame.Game
    ( Game(players, pile, deck, cardSuits, cardRanks, turnOrder,
           cardEffects, gameName, cardGen, playerMoves, endCon, winCon,
           actions, rules, canPlaceCard),
      GameState(TurnEnd, Start, TurnStart) )
import GameData.GD (GameData)
import GameData.LoadGD (loadGameData)
import CardGame.Card (Card (Card, cScore, suit), rank)
import Data.List.Extra (sortBy)
import Feature (Feature(..), Attribute (GameAttributes, CardAttributes, PlayerAttributes, Actions))
import Data.Maybe (mapMaybe)
import CardGame.Player (standardMoves, parsePlayerMovesExpr, Player (pScore, hand), parsePlayerMoves, resetMoves)
import CDSL.CDSLExpr (CDSLExpr(Numeric, Greatest, Players, Score, IsEqual, All, IsEmpty, Hand, CardValue, CardRank, CardSuit, Text, CLe, CGr, CLEq, CGRq, Null))
import CDSL.ExecCDSLExpr (execCDSLGame, execCDSLGameBool, cardFromCDSL)
import Data.CircularList (toList, fromList)
import Functions (lookupAll, lookupOrDefault, lookupM, lookupMAll)
import CDSL.ParseCDSLExpr (toNumeric)
import CardGame.CardFunctions (defaultCardSuits, defaultCardRanks, defaultCardValues, makeDeck, cardElem)
import qualified Data.Map as Map

loadGame :: Game -> Int -> IO Game
loadGame g n = do
    res <- loadGameData Map.empty n
    case res of
        Left gd -> loadGame' gd g
        Right e -> do
            print e
            return g


-- TODO: Add card effects back to the game
loadGame' :: GameData -> Game -> IO Game
loadGame' gd g = do
    -- gamename
    gm <- case Map.lookup GameAttributes gd of
        Just att -> case lookupM GameName att of
            Just (_, Text nm:_) -> return nm
            _ -> return "game"
        _ -> return "game"
    -- Cards
    (cs, cr, cv) <- case Map.lookup CardAttributes gd of
        Just att -> do
            s <- case lookupM CardSuits att of
                Just (_, suits) -> return suits
                _ -> return defaultCardSuits
            r <- case lookupM CardRanks att of
                Just (_, ranks) -> return ranks
                _ -> return defaultCardRanks
            v <- case lookupM CardValues att of
                Just (_, values) -> return (mapMaybe toNumeric values)
                _ -> return defaultCardValues
            return (s, r, v)

        Nothing -> return (defaultCardSuits, defaultCardRanks, defaultCardValues)

    -- Card comperator
    (ccI, ccS) <- case Map.lookup CardAttributes gd of
        Just att -> do
            comp <- case lookupM CardCompare att of
                Just (_, r) -> return r
                Nothing -> return []
            return (getComparatorInt comp, getComparatorString comp)
        Nothing -> return (getComparatorInt [], getComparatorString [])

    let cards' = makeDeck cs cr cv

    ce <- case Map.lookup CardAttributes gd of
        Just att -> do
            let cc = lookupMAll CEChangeCard att
            let sh = lookupMAll CESwapHand att
            let tfh = lookupMAll CETakeFromHand att
            let gc = lookupMAll CEGiveCard att
            let pn = lookupMAll CEPassNext att
            let dc = lookupMAll CEDrawCard att
            return (cc ++ sh ++ tfh ++ gc ++ pn ++ dc)
        Nothing -> return []

    to <- case Map.lookup GameAttributes gd of
        Just att -> case lookupM TurnOrder att of
            Just (_, vl) -> return vl
            _ -> return []
        Nothing -> return []

    let cg = cycle cards'

    -- Player

    mv <- case Map.lookup PlayerAttributes gd of
        Just att -> case lookupM PlayerMoves att of
            Just (_, mvs) -> return (parsePlayerMovesExpr mvs)
            _ -> return standardMoves
        Nothing -> return standardMoves


    -- Card Count
    hc <- case Map.lookup PlayerAttributes gd of
        Just att -> case lookupM PlayerHand att of
            Just (_, ex) -> return ex
            _ -> return [Numeric 3]
        Nothing -> return [Numeric 3]


    -- End con
    -- Win con
    (wc, ec) <- case Map.lookup GameAttributes gd of
        Just att -> do
            return (map snd $ lookupMAll WinCon att, map snd $ lookupMAll EndCon att)
        Nothing -> return ([], [])

    -- Card exception constraints
    cex <- case Map.lookup CardAttributes gd of
        Just att -> case lookupM ExceptionConstraints att of
            Just ((_, Just cc), [CardSuit]) -> return (Right (getComparatorString cc), CardSuit)
            Just ((_, Just cc), [CardRank]) -> return (Right (getComparatorString cc), CardRank)
            Just ((_, Just cc), [CardValue]) -> return (Left (getComparatorInt cc), CardValue)
            _ -> return (Left (\_ _ -> False), Null)
        _ -> return (Left (\_ _ -> False), Null)


    -- Can place cards
    pc <- case Map.lookup CardAttributes gd of
        Just att -> case lookupM CardConstraints att of
            Just (_, ex) -> return (placeCardStmt ex ccI ccS cex)
            Nothing -> return (placeCardStmt [] ccI ccS cex)
        _ -> return (placeCardStmt [] ccI ccS cex)



    let game = g {
        deck = cards'
        --, players = fromList (map (`resetMoves` mv) (toList (players g)))
        , cardSuits = cs
        , cardRanks = cr
        , turnOrder = to
        , cardEffects = ce
        , gameName = gm
        , cardGen = cg
        , playerMoves = mv
        , endCon = map createEndCon ec
        , winCon = map createWinCon wc
        , actions = []
        , rules = [(PlayerHand, hc)]
        , canPlaceCard = [pc]
    }

    case Map.lookup Actions gd of
        Just att -> do
            -- Rules that should be checked at specific times

            -- Anytime
            let at = map (execCDSLGame . snd) (lookupMAll AnyTime att)

            -- Start
            let st = map (execCDSLGame . snd) (lookupMAll StartTime att)

            -- TurnStart
            let ts = map (execCDSLGame . snd) (lookupMAll TurnStartTime att)

            -- TurnEnd
            let te = map (execCDSLGame . snd) (lookupMAll TurnEndTime att)
            return (game { actions = [
                (Start, map (\val -> (val, True)) at)
                , (Start, map (\val -> (val, True)) st)
                , (TurnStart, map (\val -> (val, True)) at)
                , (TurnEnd, map (\val -> (val, True)) at)
                , (TurnStart, map (\val -> (val, True)) ts)
                , (TurnEnd, map (\val -> (val, True)) te)]})
        Nothing -> return game
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
            | c `cardElem` cardFromCDSL con = True
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

                con :: [CDSLExpr]
                con = case Map.lookup CardAttributes gd of
                    Just att -> case lookupM IgnoreConstraints att of
                        Just (_, v) -> v
                        Nothing -> []
                    Nothing -> []


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


