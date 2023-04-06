module Functions where

import Data.List.Extra (splitOn)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)

{-

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
    --let mv = maybe standardMoves parsePlayerMoves (lookup PlayerMoves rls)
    -- Pile
    let pl = case lookup PileCount rls of
            Just s -> maybe "1" show (readMaybe s :: Maybe Int)
            _ -> "1"

    -- Card Count
    let hc = lookupOrDefault PlayerHand "3" rls

    -- End con
    --let ec = map (createEndCon . parseString . words) (lookupAll EndCon rls)

    -- Win con
    --let wc = map (createWinCon . parseString . words) (lookupAll WinCon rls)


    -- Can place cards
    --let pc = placeCardStmt (map (parseString . words) (lookupAll CardConstraints rls))
    --print (map (parseString . words) (lookupAll CardConstraints rls))
    --print (any (`elem` [Always]) [CardRank, CardSuit, CardValue])
    -- Rules that should be checked at specific times
    -- Anytime
    --let at = map (execIfExpr . parseIfString) (lookupAll AnyTime rls)

    -- Start
    --let st = map (execIfExpr . parseIfString) (lookupAll StartTime rls)
    return g {
        deck = cards'
        , cardGen = cg
        --, players = fromList (map (`resetMoves` mv) (toList (players g)))
        , endCon = ec
        , winCon = wc
        , actions = [(Start, at), (Start, st), (TurnStart, st), (TurnEnd, st)]
       -- , rules = [(PlayerHand, hc),(PileCount, pl), (PlayerMoves, intercalate "," (map toString mv))]
        , canPlaceCard = [pc]
    }



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



-}
-- Returns all Maybe lookups in a list
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]



-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Split a string at the comma separator and trim each element
splitAndTrim :: String -> [String]
splitAndTrim = map trim . splitOn ","



takeUntilDuplicate :: Eq a => [a] -> [a]
takeUntilDuplicate = go []
  where
    go _ [] = []
    go seen (y:ys)
      | y `elem` seen = []
      | otherwise = y : go (y:seen) ys

    
removeNth :: Int -> [a] -> (a, [a])
removeNth n xs = (xs !! n, take n xs ++ drop (n+1) xs)


-- Removes the first instance of the given element
removeFirst :: Eq a => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) y = if x == y
    then
        xs
    else
        x : removeFirst xs y


-- Removes all duplicate elements from the given list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if x `elem` xs
    then
        unique xs
    else
        x : unique xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
    where
        lesser = filter (< x) xs
        greater = filter (>= x) xs


lookupOrDefault :: Eq k => k -> a -> [(k, a)] -> a
lookupOrDefault key def assocList = fromMaybe def (lookup key assocList)




mergeList :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeList [] ys = ys
mergeList ((xk, xv):xs) ys =
  case lookup xk ys of
    Just yv -> mergeList xs ((xk, yv):ys)
    Nothing -> mergeList xs ((xk, xv):ys)



lookupMany :: Eq a => [a] -> [(a, b)] -> [b]
lookupMany keys pairs = mapMaybe (`lookup` pairs) keys

lookupManyWithKey :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupManyWithKey keys list = [(k, v) | (k, v) <- list, k `elem` keys]

splitEithers :: [(a, [Either b c])] -> ([(a, [b])], [(a, [c])])
splitEithers [] = ([], [])
splitEithers ((a, x):xs) = bimap
  ((a, fst (partitionEithers x)) :)
  ((a, snd (partitionEithers x)) :) (splitEithers xs)

