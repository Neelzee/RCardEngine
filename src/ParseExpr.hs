module ParseExpr where
import Game
import Card (shuffle, Card (Card), defaultCardSuits, defaultCardNames, defaultCardValues)
import Player (Player(..), Move (PlayCard, DrawCard, Pass), standardMoves, resetMoves)
import Data.List (sortBy, intercalate)
import Text.Read (readMaybe)
import Data.CircularList (toList, fromList)
import GameExprError (GameError (GameInvalidSyntax))
import Data.List.Extra (split)
import Data.Maybe (mapMaybe)
import GameRules (GameRule (..), parseGameRules)

data GameExpr =
    Any GameExpr
    | All GameExpr
    | Greatest GameExpr
    | Players GameExpr
    | Score
    | Hand
    | IsEqual GameExpr GameExpr
    | GValue Int
    | IsEmpty GameExpr
    | If GameExpr [GameExpr]
    | Swap GameExpr GameExpr
    | Shuffle GameExpr
    | Deck
    | Pile
    | Take GameExpr GameExpr GameExpr
    | Null
    deriving (Show)


loadGame :: FilePath -> Game -> IO Game
loadGame path g = do
    rawContent <- readFile path
    let content = split (== '\n') rawContent
    case validateGame content of
        Left rls -> return (loadGame' rls g)
        Right err -> error (show err)

loadGame' :: [(GameRule, [String])] -> Game -> Game
loadGame' rls g =
    let cs = head (lookupOrDefault CardSuits [unwords defaultCardSuits] rls)
        cn = head (lookupOrDefault CardNames [unwords defaultCardNames] rls)
        cv = head (lookupOrDefault CardValues [unwords (map show defaultCardValues)] rls)
        cards' = [ Card s n sc | s <- words cs, n <- words cn, sc <- map (\p -> read p :: Int) (words cv) ]
        -- Player
        mv = maybe standardMoves parsePlayerMoves (lookup PlayerMoves rls)
        -- Card Count
        hc = read (head (lookupOrDefault PlayerHand ["3"] rls)) :: Int

        -- End con
        ec = map (createEndCon . parseString)(lookupAll EndCon rls)

        -- Win con
        wc = map (createWinCon . parseString) (lookupAll WinCon rls)

    in g {
        cards = cards'
        , players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
    }




validateGame :: [String] -> Either [(GameRule, [String])] GameError
validateGame (x:xs) =
    case parseGameRules x of
        Just a -> do
            let (as, xs') = span (/= "END") xs
            case validateGame xs' of
                Left bs -> Left ((a, as) : bs)
                Right e -> Right e
        _ -> Right (GameInvalidSyntax "eep")
validateGame _ = Right (GameInvalidSyntax "eep")



parsePlayerMoves :: [String] -> [(Move, Bool)]
parsePlayerMoves = mapMaybe parsePlayerMove

parsePlayerMove :: String -> Maybe (Move, Bool)
parsePlayerMove x = case words x of
    ["PLAYCARD", b] -> Just (PlayCard, b == "TRUE")
    ["DRAWCARD", b] -> Just (DrawCard, b == "TRUE")
    ["PASS", b] -> Just (Pass, b == "TRUE")
    _ -> Nothing


execIfExpr :: GameExpr -> Game -> Game
execIfExpr (If (IsEmpty Deck) xs) g = if null (deck g)
  then
      foldr execExpr g xs
  else
      g
execIfExpr _ g = g


execExpr :: GameExpr -> Game -> Game
execExpr (Swap a b) g = g { deck = getDeckExpr a g, pile = getDeckExpr b g}
execExpr (Shuffle a) g = do
    let d = getDeckExpr a g
    let d' = shuffle d
    case a of
        Deck -> g { deck = d' }
        Pile -> g { pile = d' }
        _ -> g
execExpr (Take (GValue n) a b) g = do
    let pl = getDeckExpr a g
    let dk = getDeckExpr b g
    let dk' = take n pl ++ dk
    let pl' = drop n pl
    case (a, b) of
        (Pile, Deck) -> g { pile = pl', deck = dk' }
        (Deck, Pile) -> g { pile = dk', deck = pl' }
        _ -> g
execExpr _ g = g


getDeckExpr :: GameExpr -> (Game -> [Card])
getDeckExpr Deck = deck
getDeckExpr Pile = pile
getDeckExpr _ = const []

parseIfString :: ([String], [[String]]) -> GameExpr
parseIfString (xs, ys) = If (parseString xs) (map parseString ys)


parseString :: [String] -> GameExpr
parseString [] = Null
parseString (x:xs) = case x of
    "any" -> Any (parseString xs)
    "greatest" -> Greatest (parseString xs)
    "players" -> Players (parseString xs)
    "score" -> Score
    "hand" -> Hand
    "isEqual" -> IsEqual (parseString [head xs]) (parseString [xs !! 1])
    "isEmpty" -> IsEmpty (parseString xs)
    "deck" -> Deck
    "shuffle" -> Shuffle (parseString xs)
    "swap" -> Swap (parseString [head xs]) (parseString [xs !! 1])
    "pile" -> Pile
    "take" -> Take (parseString [head xs]) (parseString [xs !! 1]) (parseString [xs !! 2])
    s -> maybe Null GValue (readMaybe s :: Maybe Int)

createEndCon :: GameExpr -> (Game -> Bool)
createEndCon (Any (Players (IsEmpty Hand))) = any (null . hand) . players
createEndCon (Any (Players (IsEqual Score (GValue n)))) = any ((== n) . pScore) . players
createEndCon _ = const False


createWinCon :: GameExpr -> (Game -> [Player])
createWinCon (Any expr) = createWinCon expr
createWinCon (All expr) = createWinCon expr
createWinCon (Greatest (Players Score)) = sortBy (\p1 p2  -> compare (pScore p1) (pScore p2)) . toList . players
createWinCon (Players (IsEqual Score (GValue n)))= sortBy (\p1 p2 -> compare (distance n (pScore p1)) (distance n (pScore p2))) . toList . players
    where distance x y = abs (x - y)
createWinCon (Players (IsEmpty Hand))= sortBy (\p1 p2 -> compare (length $ hand p1) (length $ hand p2)) . toList . players
createWinCon _ = const []



lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]