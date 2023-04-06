module LoadGame where
import Game
import GD (GameData)
import LoadGD (loadGameData)
import Card (defaultCardSuits, defaultCardValues, makeDeck)
import Data.List.Extra (splitOn, trim)
import Feature (Feature(CardSuits, CardRanks, CardValues, PlayerMoves, PileCount, PlayerHand, EndCon, WinCon, CardConstraints, AnyTime, StartTime))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Player (standardMoves, resetMoves, parsePlayerMovesExpr)
import CDSLExpr (CDSLExpr(Numeric))
import ExecCDSLExpr (execCDSLGame)
import Data.CircularList (toList, fromList)

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
    let cs = maybe defaultCardSuits (map trim . splitOn "," . show) (lookup CardSuits rls)
    let cn = maybe defaultCardSuits (map trim . splitOn "," . show) (lookup CardRanks rls)
    let cv = maybe defaultCardValues (map ((fromMaybe 0 . readMaybe) . trim) . splitOn "," . show) (lookup CardValues rls)
    let cards' = makeDeck cs cn cv
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


    -- Can place cards
    let pc = placeCardStmt (lookupAll CardConstraints rls)
    -- Rules that should be checked at specific times
    -- Anytime
    let at = map execCDSLGame (lookupAll AnyTime rls)

    -- Start
    let st = map execCDSLGame (lookupAll StartTime rls)
    return g {
        deck = cards'
        , cardGen = cg
        , playerMoves = mv
        , players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
        , actions = [(Start, at), (Start, st), (TurnStart, st), (TurnEnd, st)]
        , rules = [(PlayerHand, hc),(PileCount, pl)]
        , canPlaceCard = [pc]
    }

placeCardStmt :: [[CDSLExpr]] -> t
placeCardStmt = undefined

createEndCon :: [CDSLExpr] -> b
createEndCon = undefined

createWinCon :: [CDSLExpr] -> b
createWinCon = undefined



-- Returns all Maybe lookups in a list
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]