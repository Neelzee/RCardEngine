module Terminal.ValidateGameCommands (
    validateGameCommand
    , validateGCFlags
    ) where

import Terminal.GameCommands (GCError (GCError, errType, UnknownFlagsError, input, UnknownCommandError, InvalidCommandArgumentError, CDSLError, IncompleteCommandError), GameCommand (Help, Create, Save, cmd, Edit, Add, Update, Test, Remove, Copy, Rename, Status, Close, Clear, Quit, List, Play, Debug), Flag, commands)
import Data.List.Extra (trim, sortOn, partition, groupBy)
import CDSL.CDSLExpr (CDSLExpr(Text, Numeric))
import Text.Read (readMaybe)
import Feature (fromStringToFeature)
import CDSL.ParseCDSLExpr (parseExpr, parseOneCDSL)


validateGameCommand :: String -> Either GameCommand GCError
validateGameCommand xs = case map trim (words xs) of
    ("debug":y:ys) -> case (readMaybe y :: Maybe Int, validateGCFlags ys) of
        (Just i, Left flgs) -> Left (Debug (Numeric i) flgs)
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })
        (_, Right e) -> Right e
    ["debug"] -> Right (GCError { errType = IncompleteCommandError, input = xs})
    ("create":gm:ys) -> case validateGCFlags ys of
        Left flgs -> Left (Create (Text gm) flgs)
        Right e -> Right e
    ["create"] -> Right (GCError { errType = IncompleteCommandError, input = xs})

    ("save":ys) -> case validateGCFlags ys of
        Left flgs -> Left (Save flgs)
        Right e -> Right e

    ("edit":y:ys) -> case (readMaybe y :: Maybe Int, validateGCFlags ys) of
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })
        (_, Right e) -> Right e
        (Just i, Left flgs) -> Left (Edit (Numeric i) flgs)
    ["edit"] -> Right (GCError { errType = IncompleteCommandError, input = xs})

    ("add":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case parseExpr exs of
                Left exps -> Left (Add f exps flg)
                Right errs -> Right (GCError { errType = CDSLError (Right errs), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ feature ++ "' instead."), input = xs })
    ["add"] -> Right (GCError { errType = IncompleteCommandError, input = xs})

    ("update":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case parseExpr exs of
                Left exps -> Left (Update f exps flg)
                Right errs -> Right (GCError { errType = CDSLError (Right errs), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ feature ++ "' instead."), input = xs })
    ["update"] -> Right (GCError { errType = IncompleteCommandError, input = xs})


    ("test":feature:ys) -> case fromStringToFeature feature of
        (Just f) -> do
            let input = unwords ys
            let (exprStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let exs = map unwords $ groupBy (\_ b -> head b /= '-') exprStr
            let flg = map tail flagStrs
            case parseExpr exs of
                Left exps -> Left (Test f exps flg)
                Right errs -> Right (GCError { errType = CDSLError (Right errs), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ feature ++ "' instead."), input = xs })
    ["test"] -> Right (GCError { errType = IncompleteCommandError, input = xs})

    ("remove":ys) -> case (mapM fromStringToFeature (words (takeWhile (/= '-') (unwords ys))), validateGCFlags (words (dropWhile (/= '-') (unwords ys)))) of
        (Just fets, Left flg) -> Left (Remove fets flg)
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a Feature, but got '" ++ xs ++ "' instead."), input = xs })
        (_, Right e) -> Right e
    ["remove"] -> Right (GCError { errType = IncompleteCommandError, input = xs})



    ("copy":y:ys)  -> case readMaybe y :: Maybe Int of
        (Just i) -> do
            let input = unwords ys
            let (fetStr, flagStrs) = partition (\s -> head s /= '-') (words input)
            let fs = map unwords $ groupBy (\_ b -> head b /= '-') fetStr
            case (mapM fromStringToFeature fs, map tail flagStrs) of
                (Just fets, flg) -> Left (Copy (Numeric i) fets flg)
                (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected any features, got '" ++ unwords fs ++ "' instead."), input = xs })
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })
    ["copy"] -> Right (GCError { errType = IncompleteCommandError, input = xs})

    ("rename":y:nm:ys) -> case (readMaybe y :: Maybe Int, validateGCFlags ys) of
        (Just i, Left flgs) -> Left (Rename (Numeric i) (Text nm) flgs)
        (Nothing, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ y ++ "' instead."), input = xs })
        (_, Right e) -> Right e
    ["rename"] -> Right (GCError { errType = IncompleteCommandError, input = xs})


    ("status":ys) -> case validateGCFlags ys of
        Left flgs -> Left (Status flgs)
        Right e -> Right e
    ["status"] -> Right (GCError { errType = IncompleteCommandError, input = xs})


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

    ["help"] -> Left (Help Nothing)
    ["help","commands"] -> Left (Help Nothing)
    ["help","feature"] -> Left (Help (Just "feature"))
    ["help","cdsl"] -> Left (Help (Just "cdsl"))
    ["help","flags"] -> Left (Help (Just "flags"))
    ("help":ys) -> if length ys == 1
        then
            case validateGCFlags ys of
                Left _ -> Left (Help (Just (unwords ys))) -- Give information about that flag
                Right _ -> case validateGameCommand (unwords ys) of
                    Left _ -> Left (Help (Just (unwords ys))) -- Give information about that game command
                    Right _ -> case fromStringToFeature (unwords ys) of
                        Just _ -> Left (Help (Just (unwords ys)))
                        Nothing -> case parseOneCDSL (unwords ys : repeat " null") 0 of
                            Right _ -> Right (GCError { errType = InvalidCommandArgumentError ("Could not find any information about " ++ unwords ys), input = xs })
                            Left (Text _, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Could not find any information about " ++ unwords ys), input = xs })
                            Left (Numeric _, _) -> Right (GCError { errType = InvalidCommandArgumentError ("Could not find any information about " ++ unwords ys), input = xs })
                            Left _ -> Left (Help (Just (unwords ys))) -- Give information about that expression
        else
            Right (GCError { errType = InvalidCommandArgumentError ("Can only give information about one thing at the time, '" ++ unwords ys ++ "'"), input = xs })
    
    ("play":ys) -> case readMaybe (unwords ys) :: Maybe Int of
        Just i -> Left (Play (Numeric i))
        Nothing -> Right (GCError { errType = InvalidCommandArgumentError ("Expected a number, got '" ++ unwords ys ++ "' instead."), input = xs })
    ["play"] -> Right (GCError { errType = IncompleteCommandError, input = xs})
    

    _ -> case suggestCorrection (head $ words xs) commands of
        Just pge -> Right (GCError { errType = UnknownCommandError ("'" ++ xs ++ "'is not a valid command. Did you mean: " ++ pge ++ "?"), input = xs })
        Nothing -> Right (GCError { errType = UnknownCommandError ("'" ++ xs ++ " 'is not a valid command"), input = xs })



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