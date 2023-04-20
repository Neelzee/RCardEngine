module Terminal.ValidateGameCommands (
    validateGameCommand
    , validateGCFlags
    ) where

import Terminal.GameCommands (GCError (GCError, errType, UnknownFlagsError, input, UnknownCommandError, InvalidCommandArgumentError, CDSLError), GameCommand (Help, Create, Save, cmd, Edit, Add, Update, Test, Remove, Copy, Rename, Status, Close, Clear, Quit, List, Play), Flag, commands)
import Data.List.Extra (trim, sort, sortOn, partition, groupBy)
import CDSL.CDSLExpr (CDSLExpr(Text, Numeric))
import Text.Read (readMaybe)
import Feature (fromStringToFeature)
import Data.Either (partitionEithers)
import CDSL.ParseCardDSL (parseExpr)


validateGameCommand :: String -> Either GameCommand GCError
validateGameCommand xs = case map trim (words xs) of
    ("create":gm:ys) -> case validateGCFlags ys of
        Left flgs -> Left (Create (Text gm) flgs)
        Right e -> Right e

    ("save":ys) -> case validateGCFlags ys of
        Left flgs -> Left (Save flgs)
        Right e -> Right e

    ("edit":y:ys) -> case (readMaybe y :: Maybe Int, validateGCFlags ys) of
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })
        (_, Right e) -> Right e
        (Just i, Left flgs) -> Left (Edit (Numeric i) flgs)

    ("add":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case partitionEithers (map (parseExpr . words) exs) of
                (exps, []) -> Left (Add f (concat exps) flg)
                (_, errs) -> Right (GCError { errType = CDSLError (Right (concat errs)), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ feature ++ "' instead."), input = xs })

    ("update":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case partitionEithers (map (parseExpr . words) exs) of
                (exps, []) -> Left (Update f (concat exps) flg)
                (_, errs) -> Right (GCError { errType = CDSLError (Right (concat errs)), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ feature ++ "' instead."), input = xs })

    ("test":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case partitionEithers (map (parseExpr . words) exs) of
                (exps, []) -> Left (Test f (concat exps) flg)
                (_, errs) -> Right (GCError { errType = CDSLError (Right (concat errs)), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ feature ++ "' instead."), input = xs })

    ("remove":ys) -> case (mapM fromStringToFeature (words (takeWhile (/= '-') (unwords ys))), validateGCFlags (words (dropWhile (/= '-') (unwords ys)))) of
        (Just fets, Left flg) -> Left (Remove fets flg)
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ xs ++ "' instead."), input = xs })
        (_, Right e) -> Right e

    ("copy":y:ys)  -> case readMaybe y :: Maybe Int of
        (Just i) -> do
            let input = unwords ys
            let (fetStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let fs = map unwords $ groupBy (\_ b -> head b /= '-') fetStr
            case (mapM fromStringToFeature fs, map tail flagStrs) of
                (Just fets, flg) -> Left (Copy (Numeric i) fets flg)
                (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected any features, got '" ++ unwords fs ++ "' instead."), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })

    ("rename":y:nm:ys) -> case (readMaybe y :: Maybe Int, validateGCFlags ys) of
        (Just i, Left flgs) -> Left (Rename (Numeric i) (Text nm) flgs)
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })
        (_, Right e) -> Right e

    ("status":ys) -> case validateGCFlags ys of
        Left flgs -> Left (Status flgs)
        Right e -> Right e

    ("close":ys) -> case validateGCFlags ys of
        Left flgs -> Left (Close flgs)
        Right e -> Right e

    ("quit":ys) -> case validateGCFlags ys of
        Left flgs -> Left (Quit flgs)
        Right e -> Right e

    ["clear"] -> Left Clear

    ("list":ys) -> case validateGCFlags ys of
        Left flgs -> Left (List flgs)
        Right e -> Right e

    ["help"] -> Left Help

    ("play":ys) -> case readMaybe (unwords ys) :: Maybe Int of
        Just i -> Left (Play (Numeric i))
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ unwords ys ++ "' instead."), input = xs })

    _ -> case suggestCorrection (head $ words xs) commands of
        Just pge -> Right (GCError { errType = UnknownCommandError ("'" ++ xs ++ "'is not a valid command. Did you mean: " ++ pge ++ "?"), input = xs })
        Nothing -> Right (GCError { errType = UnknownCommandError ("'" ++ xs ++ "'is not a valid command"), input = xs })



-- Compute the Levenshtein distance between two strings
levenshteinDistance :: Eq a => [a] -> [a] -> Int
levenshteinDistance xs ys = lev (length xs) (length ys)
    where
        lev i 0 = i
        lev 0 j = j
        lev i j
            | xs !! (i - 1) == ys !! (j - 1) = lev (i - 1) (j - 1)
            | otherwise = minimum [1 + lev i (j - 1), 1 + lev (i - 1) j, 1 + lev (i - 1) (j - 1)]


findMatches :: String -> [String] -> Maybe [String]
findMatches wrd wrds =
    do
        let distance = levenshteinDistance
        let closeWords = filter (\w -> distance w wrd <= 2) wrds
        case closeWords of
            [] -> Nothing
            _ -> Just $ take 10 $ map fst $ sortOn snd $ map (\w -> (w, distance w wrd)) closeWords


-- Suggest a correction for the given command
suggestCorrection :: String -> [GameCommand] -> Maybe String
suggestCorrection inp cmds = do
    case findMatches inp (map (show . cmd) cmds) of
        Just (x:_) -> Just x
        _ -> Nothing


validateGCFlags :: [String] -> Either Flag GCError
validateGCFlags [] = Left []
validateGCFlags (x:xs) = case x of
    "-confirm" -> case validateGCFlags xs of
        Left lst -> Left ("-confirm":lst)
        Right e -> Right e
    "-verbose" -> case validateGCFlags xs of
        Left lst -> Left ("-verbose":lst)
        Right e -> Right e
    "-quiet" -> case validateGCFlags xs of
        Left lst -> Left ("-quiet":lst)
        Right e -> Right e
    e -> Right (GCError { errType = UnknownFlagsError, input = e })