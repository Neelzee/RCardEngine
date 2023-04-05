module ParseExpr where

import Game
    ( lookupOrDefault,
      Game(..),
      GameState(Start, TurnEnd, TurnStart) )
import Card (shuffle, defaultCardSuits, defaultCardValues, makeDeck, Card (rank, cScore, suit))
import Player (Player(..), Move (PlayCard, DrawCard, Pass), standardMoves, resetMoves, toString)
import Data.List (sortBy, intercalate, elemIndex)
import Data.CircularList (toList, fromList)
import Data.List.Extra (split, splitOn)
import Data.Maybe ( mapMaybe, isJust )
import GameRules (GameRule (..), parseGameRules)
import GameExprError (GameError (MultipleLinesInStatement, MissingTerminationStatement, UnknownKeyWord))
import Text.Read (readMaybe)
import Data.Char (isSpace)
import System.Directory.Internal.Prelude (fromMaybe)

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
    | Always
    | CardRank
    | CardSuit
    | CardValue
    deriving (Eq, Show, Ord)


loadGame :: String -> Game -> IO Game
loadGame gamename g = do
    rawContent <- readFile ("games/" ++ gamename)
    let content = lines rawContent
    case parseFile content of
        Left rls -> do
            g' <-loadGame' rls g
            return g' { gameName = gamename }
        Right err -> error (show err)

loadGame' :: [(GameRule, String)] -> Game -> IO Game
loadGame' rls g = do
    let cs = maybe defaultCardSuits splitAndTrim (lookup CardSuits rls)
    let cn = maybe defaultCardSuits splitAndTrim (lookup CardRanks rls)
    let cv = maybe defaultCardValues (map (fromMaybe 0 . readMaybe) . splitAndTrim) (lookup CardValues rls)
    let cards' = makeDeck cs cn cv
    let cg = cycle cards'
    -- Player
    let mv = maybe standardMoves parsePlayerMoves (lookup PlayerMoves rls)
    -- Pile
    let pl = case lookup PileCount rls of
            Just s -> maybe "1" show (readMaybe s :: Maybe Int)
            _ -> "1"

    -- Card Count
    let hc = lookupOrDefault PlayerHand "3" rls

    -- End con
    let ec = map (createEndCon . parseString . words) (lookupAll EndCon rls)

    -- Win con
    let wc = map (createWinCon . parseString . words) (lookupAll WinCon rls)


    -- Can place cards
    let pc = placeCardStmt (map (parseString . words) (lookupAll CardConstraints rls))
    print (map (parseString . words) (lookupAll CardConstraints rls))
    print (any (`elem` [Always]) [CardRank, CardSuit, CardValue])
    -- Rules that should be checked at specific times
    -- Anytime
    let at = map (execIfExpr . parseIfString) (lookupAll AnyTime rls)

    -- Start
    let st = map (execIfExpr . parseIfString) (lookupAll StartTime rls)
    return g {
        deck = cards'
        , cardGen = cg
        , players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
        , actions = [(Start, at), (Start, st), (TurnStart, st), (TurnEnd, st)]
        , rules = [(PlayerHand, hc),(PileCount, pl), (PlayerMoves, intercalate "," (map toString mv))]
        , canPlaceCard = [pc]
    }

placeCardStmt :: [GameExpr] -> (Game -> Card -> Bool)
placeCardStmt [] = const . const True
placeCardStmt [Null] = const . const True
placeCardStmt xs
    | any (`elem` xs) [CardRank, CardSuit, CardValue] = compareCards xs
    | otherwise = const . const False


compareCards :: [GameExpr] -> Game -> Card -> Bool
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



takeUntilDuplicate :: Eq a => [a] -> [a]
takeUntilDuplicate = go []
  where
    go _ [] = []
    go seen (y:ys)
      | y `elem` seen = []
      | otherwise = y : go (y:seen) ys

evalExpr :: GameExpr -> Game -> Maybe Bool
evalExpr (Any (Players (IsEmpty a))) g = case a of
    Hand -> Just (any (null . hand) (toList (players g)))
    _ -> Nothing
evalExpr (Any (Players (IsEqual a b))) g = case (calcExpr a g, calcExpr b g) of
    (Just _, Just _) -> Nothing
    (Just x, _) -> case b of
        Hand -> Just (any ((== x) . length . hand) (toList (players g)))
        Score -> Just (any ((== x) . pScore) (toList (players g)))
        _ -> Nothing
    (_, Just y) -> case a of
        Hand -> Just (any ((== y) . length . hand) (toList (players g)))
        Score -> Just (any ((== y) . pScore) (toList (players g)))
        _ -> Nothing
    _ -> Nothing
evalExpr (All (Players (IsEmpty a))) g = case a of
    Hand -> Just (all (null . hand) (toList (players g)))
    _ -> Nothing
evalExpr (IsEmpty a) g = case getMaybeDeckExpr a of
    Just expr -> Just (null (expr g))
    Nothing -> Nothing
evalExpr Always _ = Just True
evalExpr (IsEqual a b) g = case (calcExpr a g, calcExpr b g) of
    (Just x, Just y) -> Just (x == y)
    _ -> Nothing
evalExpr _ _ =  Nothing


calcExpr :: GameExpr -> Game -> Maybe Int
calcExpr (GValue n) _ = Just n
calcExpr expr g = case getMaybeDeckExpr expr of
    Just f -> Just (length (f g))
    Nothing -> Nothing


getMaybeDeckExpr :: GameExpr -> Maybe (Game -> [Card])
getMaybeDeckExpr Deck = Just deck
getMaybeDeckExpr Pile = Just pile
getMaybeDeckExpr _ = Nothing

-- Takes in a string, and converts it to a GameExpr
parseIfString :: String -> GameExpr
parseIfString s = case split ( == ':') s of
    [condition, expr] ->
        if null expr
            then
                Null
            else
                If (parseString (words condition)) (map (parseString . words) (splitAndTrim (drop 1 expr)))
    _ -> Null

-- Takes in a list of words, and converts it to a single GameExpr
parseString :: [String] -> GameExpr
parseString [] = Null
parseString (x:xs) = case x of
    "any" -> Any (parseString xs)
    "players" -> Players (parseString xs)
    "isEqual" -> IsEqual (parseString [head xs]) (parseString [xs !! 1])
    "score" -> Score
    "hand" -> Hand
    "greatest" -> Greatest (parseString xs)
    "deck" -> Deck
    "isEmpty" -> IsEmpty (parseString xs)
    "swap" -> Swap (parseString [head xs]) (parseString [xs !! 1])
    "take" -> Take (parseString [head xs]) (parseString [xs !! 1]) (parseString [xs !! 2])
    "pile" -> Pile
    "shuffle" -> Shuffle (parseString xs)
    "always" -> Always
    "rank" -> CardRank
    "suit" -> CardSuit
    "value" -> CardValue
    s -> maybe Null GValue (readMaybe s :: Maybe Int)





parsePlayerMoves :: String -> [(Move, Bool)]
parsePlayerMoves s = mapMaybe parsePlayerMove (splitAndTrim s)

parsePlayerMove :: String -> Maybe (Move, Bool)
parsePlayerMove x = case words x of
    ["PLAYCARD", b] -> Just (PlayCard, b == "TRUE")
    ["DRAWCARD", b] -> Just (DrawCard, b == "TRUE")
    ["PASS", b] -> Just (Pass, b == "TRUE")
    _ -> Nothing

-- Executes an if expression, if a statement is true
execIfExpr :: GameExpr -> Game -> Game
execIfExpr (If (IsEmpty Deck) xs) g = if null (deck g)
  then
      foldr execExpr g xs
  else
      g
execIfExpr (If Always xs) g = foldr execExpr g xs
execIfExpr _ g = g

-- Executes an expresion
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
execExpr expr@(If _ _) g = execIfExpr expr g -- Ensures we can have nested if-statements
execExpr _ g = g

-- Gets the correct pile or deck function
getDeckExpr :: GameExpr -> (Game -> [Card])
getDeckExpr Deck = deck
getDeckExpr Pile = pile
getDeckExpr _ = const []


-- Creates an endcon based on the given game expr
createEndCon :: GameExpr -> (Game -> Bool)
createEndCon (Any (Players (IsEmpty Hand))) = any (null . hand) . players
createEndCon (Any (Players (IsEqual Score (GValue n)))) = any ((== n) . pScore) . players
createEndCon _ = const False


-- Creates a wincon based on the given gameexpr
createWinCon :: GameExpr -> (Game -> [Player])
createWinCon (Any expr) = createWinCon expr
createWinCon (All expr) = createWinCon expr
createWinCon (Greatest (Players Score)) = sortBy (\p1 p2  -> compare (pScore p1) (pScore p2)) . toList . players
createWinCon (Players (IsEqual Score (GValue n)))= sortBy (\p1 p2 -> compare (distance n (pScore p1)) (distance n (pScore p2))) . toList . players
    where distance x y = abs (x - y)
createWinCon (Players (IsEmpty Hand))= sortBy (\p1 p2 -> compare (length $ hand p1) (length $ hand p2)) . toList . players
createWinCon _ = const []


-- Returns all Maybe lookups in a list
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]


parseFile :: [String] -> Either [(GameRule, String)] GameError
parseFile input = parseFileHelper input 1

-- TODO: Make generic
parseFileHelper :: [String] -> Int -> Either [(GameRule, String)] GameError
parseFileHelper [] _ = Left []
parseFileHelper (('#':_):xs) n = parseFileHelper xs (n + 1) -- Ignores comments
parseFileHelper ("":xs) n = parseFileHelper xs (n + 1)
parseFileHelper (x:xs) n = case parseGameRules x of
    Just rule -> do
        let (stmt, rest) = break isEnd xs
        if onlyNothing (map parseGameRules stmt)
            then -- Adding two, since we're skiping both the feature, and the end
                case parseFileHelper (drop 1 rest) (n + 2 + length stmt) of
                    Left rs -> Left ((rule, unwords stmt):rs)
                    e -> e
            else
                Right (MissingTerminationStatement ("Missing termination statement around line " ++ show n ++ ", '" ++ x ++ "'"))
    _ -> Right (UnknownKeyWord ("Unknown gamerule at line " ++ show n ++ ", '" ++ x ++ "'"))
    where
        isEnd y = take 3 y == "END"
        onlyNothing [] = True
        onlyNothing (y:ys) = case y of
            Just _ -> False
            Nothing -> onlyNothing ys




-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Split a string at the comma separator and trim each element
splitAndTrim :: String -> [String]
splitAndTrim = map trim . splitOn ","




validateGameExpr :: GameExpr -> Bool
validateGameExpr (Any expr) = validateGameExpr expr
validateGameExpr (All expr) = validateGameExpr expr
validateGameExpr (Players (IsEmpty Hand)) = True
validateGameExpr (Players (IsEqual Score (GValue _))) = True
validateGameExpr (Players (IsEqual Hand (GValue _))) = True
validateGameExpr (Shuffle Deck) = True
validateGameExpr (Shuffle Pile) = True
validateGameExpr (Greatest (Players Score)) = True
validateGameExpr (Greatest (Players Hand)) = True
validateGameExpr (IsEmpty Deck) = True
validateGameExpr (IsEmpty Hand) = True
validateGameExpr (IsEmpty Pile) = True
validateGameExpr (Swap a b) = (a == Pile || a == Deck) && (b == Pile || b == Deck)
validateGameExpr (Take (GValue n) a b) = (a == Pile || a == Deck) && (b == Pile || b == Deck) && n > 0
validateGameExpr Always = True
validateGameExpr (GValue _) = True
validateGameExpr _ = False

