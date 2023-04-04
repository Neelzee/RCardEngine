module ParseExpr where

import Game
    ( lookupOrDefault,
      Game(endCon, winCon, actions, rules, deck, pile, players, gameName),
      GameState(Start, TurnEnd, TurnStart) )
import Card (shuffle, defaultCardSuits, defaultCardValues, makeDeck, Card)
import Player (Player(..), Move (PlayCard, DrawCard, Pass), standardMoves, resetMoves, toString)
import Data.List (sortBy, intercalate)
import Data.CircularList (toList, fromList)
import Data.List.Extra (split, splitOn)
import Data.Maybe (mapMaybe)
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
    deriving (Eq, Show)


loadGame :: String -> Game -> IO Game
loadGame gamename g = do
    rawContent <- readFile ("games/" ++ gamename)
    let content = split (== '\n') rawContent
    case parseFile content of
        Left rls -> do
            g' <-loadGame' rls g
            return g' { gameName = gamename }
        Right err -> error (show err)

loadGame' :: [(GameRule, String)] -> Game -> IO Game
loadGame' rls g = do
    let cs = maybe defaultCardSuits splitAndTrim (lookup CardSuits rls)
    let cn = maybe defaultCardSuits splitAndTrim (lookup CardNames rls)
    let cv = maybe defaultCardValues (map (fromMaybe 0 . readMaybe) . splitAndTrim) (lookup CardValues rls)
    let cards' = makeDeck cs cn cv
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
    let t = map (parseString . words) (lookupAll WinCon rls)
    print (map words (lookupAll WinCon rls))
    print t
    -- Rules that should be checked at specific times
    -- Anytime
    let at = map (execIfExpr . parseIfString) (lookupAll AnyTime rls)

    -- Start
    let st = map (execIfExpr . parseIfString) (lookupAll StartTime rls)
    return g {
        deck = cards'
        , players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
        , actions = [(Start, at), (Start, st), (TurnStart, st), (TurnEnd, st)]
        , rules = [(PlayerHand, hc),(PileCount, pl), (PlayerMoves, intercalate "," (map toString mv))]
    }

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
parseFile input = parseFileHelper [] input 1

-- TODO: Make generic
parseFileHelper :: [(GameRule, String)] -> [String] -> Int -> Either [(GameRule, String)] GameError
parseFileHelper result [] _ = Left result
parseFileHelper result input@(x:xs) lineNumber
  | isCommentOrEmptyLine x = parseFileHelper result xs (lineNumber+1)
  | otherwise = case parseGameRules x of
      Just rule -> do
        let (linesInStatement, rest) = break isEndStatement xs
        case linesInStatement of
          [] -> Right $ MissingTerminationStatement ("Missing termination statement on line " ++ show lineNumber)
          [statement] -> parseFileHelper ((rule, statement):result) (drop 1 rest) (lineNumber + length linesInStatement + 2)
          _ -> Right $ MultipleLinesInStatement ("Multiple lines in statement starting at line " ++ show lineNumber)
      Nothing -> Right $ UnknownKeyWord ("Unknown keyword at line " ++ show lineNumber)
  where
    isEndStatement line = case parseGameRules line of
      Just _ -> False
      Nothing -> null line || head (words line) == "END"
    isCommentOrEmptyLine line = null line || head line == '#'



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

