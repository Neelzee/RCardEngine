module ParseExpr where

import Game
    ( lookupOrDefault,
      Game(cards, endCon, winCon, actions, rules, deck, pile, players, gameName),
      GameState(Start) )
import Card (shuffle, Card (Card), defaultCardSuits, defaultCardNames, defaultCardValues)
import Player (Player(..), Move (PlayCard, DrawCard, Pass), standardMoves, resetMoves)
import Data.List (sortBy)
import Data.CircularList (toList, fromList)
import Data.List.Extra (split)
import Data.Maybe (mapMaybe, isJust)
import GameRules (GameRule (..), parseGameRules)
import GameExprError (GameError (MultipleLinesInStatement, MissingTerminationStatement, UnknownKeyWord))

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


loadGame :: String -> Game -> IO Game
loadGame gamename g = do
    rawContent <- readFile ("games/" ++ gamename)
    let content = split (== '\n') rawContent
    case validateGame content of
        Left rls -> return ((loadGame' rls g) { gameName = gamename })
        Right err -> error (show err)

loadGame' :: [(GameRule, String)] -> Game -> Game
loadGame' rls g =
    let cs = lookupOrDefault CardSuits (unwords defaultCardSuits) rls
        cn = lookupOrDefault CardNames (unwords defaultCardNames) rls
        cv = lookupOrDefault CardValues (unwords (map show defaultCardValues)) rls
        cards' = [ Card s n sc | s <- words cs, n <- words cn, sc <- map (\p -> read p :: Int) (words cv) ]
        -- Player
        mv = case lookup PlayerMoves rls of
                Just m -> parsePlayerMoves (words m)
                Nothing -> standardMoves
        -- Card Count
        hc = lookupOrDefault PlayerHand "3" rls

        -- End con
        ec = map (createEndCon . parseString . words) (lookupAll EndCon rls)

        -- Win con
        wc = map (createWinCon . parseString . words) (lookupAll WinCon rls)

        -- Rules that should be checked at specific times
        at = map (execIfExpr . parseIfString . words) (lookupAll AnyTime rls)
    in g {
        cards = cards'
        , players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
        , actions = [(Start, at)]
        , rules = [(PlayerHand, hc)]
    }

parseIfString :: [String] -> GameExpr
parseIfString = undefined

parseString :: [String] -> GameExpr
parseString = undefined




validateGame :: [String] -> Either [(GameRule, String)] GameError
validateGame (x:xs) = undefined



parsePlayerMoves :: [String] -> [(Move, Bool)]
parsePlayerMoves = mapMaybe parsePlayerMove

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








