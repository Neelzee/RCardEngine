module GameData.LoadGD (loadGameData, loadGameData') where

import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Feature (Feature (GameName, CardSuits, CardRanks), isAFeatureOf, validateAttribute, Attribute (GameAttributes))
import CDSL.ParseCDSLExpr (readCDSL)
import GameData.GD (GameData)
import Functions (allGames, trim)
import CDSL.CDSLValidater (validateCDSLExpression)
import Data.List.Extra (splitOn)
import CDSL.CDSLExpr
import CardGame.CardFunctions
import Data.Bifunctor (second)
import qualified Data.Map as Map
import Data.Char (isSpace)


loadGameData :: GameData -> Int -> IO (Either GameData (CDSLParseError, Int))
loadGameData gd n = do
    g <- allGames
    let gm = takeWhile (/= '.') (g !! n)
    if n < 0 || n >= length g
    then -- Shouldnt happen
        return (Right (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show n }, 0))
    else
        do
            content <- readFile (gameFolder ++ "/" ++ (g !! n))
            case loadGameData' gd content of
                Left gamedata -> do
                    case Map.lookup GameAttributes gamedata of
                        Just att -> return (Left (Map.insert GameAttributes (Map.insert (GameName, Nothing) [Text gm] att) gamedata))
                        Nothing -> return (Left (Map.insert GameAttributes (Map.fromList [((GameName, Nothing), [Text gm])]) gamedata))
                e -> return e


loadGameData' :: GameData -> String -> Either GameData (CDSLParseError, Int)
loadGameData' oldGD c = case parseFile c of
    Left nGd -> case validateFeatureAttributes nGd of
        Left gamedata -> case validateExpr gamedata of
            Left gd -> case validateGameData gamedata of
                Nothing -> Left (Map.fromList (map (second Map.fromList) gd) `Map.union` oldGD)
                Just (e, n) -> Right (e, n)
            Right e -> Right e
        Right e -> Right e
    Right e -> Right e

    where
        validateGameData :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] -> Maybe (CDSLParseError, Int)
        validateGameData gd = case (lookup CardSuits (concatMap (\(_, a, _, _) -> map (\((x, _), y) -> (x, y)) a ) gd)
            , lookup CardRanks (concatMap (\(_, a, _, _) -> map (\((x, _), y) -> (x, y)) a ) gd)) of
            (Just suits, Just ranks) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd)
                where
                    parse :: [([CDSLExpr], Int, Int)] -> Maybe (CDSLParseError, Int)
                    parse [] = Nothing
                    parse ((ys, st, _):xs) = case mapMaybe validate ys of -- Not on exact line, but close enough
                        [] -> parse xs
                        e -> Just (head e, st)


                    validate :: CDSLExpr -> Maybe CDSLParseError
                    validate ce@(CEffect _ crds) = case filter (\crd -> not (cardElem crd (makeDeck suits ranks []))) crds of
                        [] -> Nothing
                        ex -> Just (CDSLParseError { pErr = MissMatchCardError ex (makeDeck suits ranks []), pExpr = ce, rawExpr = "" })
                    validate _ = Nothing
            e -> error (show e)



        validateExpr :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] ->
            Either [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])])] (CDSLParseError, Int)
        validateExpr [] = Left []
        validateExpr ((att, fets, st, _):xs) = case parse fets st of
            Just e -> Right e
            Nothing -> case validateExpr xs of
                Left f -> Left ((att, fets):f)
                e -> e
            where
                parse :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> Int -> Maybe (CDSLParseError, Int)
                parse [] _ = Nothing
                parse ((_, []):fs) n = parse fs n
                parse ((f, y:ys):fs) n = case validateCDSLExpression y of
                    Left _ -> parse ((f, ys):fs) (n + 1)
                    Right e -> Just (CDSLParseError { pErr = OnValidateFeature (fst f) e, pExpr = Null, rawExpr = "" }, n)

        validateFeatureAttributes :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] ->
            Either [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] (CDSLParseError, Int)
        validateFeatureAttributes [] = Left []
        validateFeatureAttributes (x@(a, fets, st, _):xs) = case parse a fets st of
            Just e -> Right e
            Nothing -> case validateFeatureAttributes xs of
                Left f -> Left (x:f)
                e -> e
            where
                parse :: Attribute -> [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> Int -> Maybe (CDSLParseError, Int)
                parse _ [] _ = Nothing
                parse att (((f, _), _):fes) n = if f `isAFeatureOf` att
                    then
                        parse att fes (n + 1)
                    else
                        Just (CDSLParseError { pErr = NotAFeatureOfAttribute att f, pExpr = Null, rawExpr = "" }, n)




removeComments :: [String] -> [String]
removeComments [] = []
removeComments (('#':_):xs) = "" : removeComments xs
removeComments (y:ys) = rmc y : removeComments ys
    where
        rmc :: String -> String
        rmc "" = ""
        rmc (z:zs)
            | z == '#' = ""
            | otherwise = z : rmc zs


parseFile :: String -> Either [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] (CDSLParseError, Int)
parseFile xs = case parseAttributes (removeComments $ lines xs) 1 of
    Left ys -> parse ys
    Right e -> Right e
    where
        parse :: [(Attribute, [String], Int, Int)] ->
            Either [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] (CDSLParseError, Int)
        parse [] = Left []
        parse ((at, ys, st, ed):zs) = case parseFeatures (concatFeatures (unlines ys)) st of
            Left fet -> case parse zs of
                Left as -> Left ((at, fet, st, ed):as)
                Right e -> Right e
            Right e -> Right e




parseAttributes :: [String] -> Int ->
    Either [(Attribute, [String], Int, Int)] (CDSLParseError, Int)
parseAttributes [] _ = Left []
parseAttributes ("":xs) n = parseAttributes xs (n + 1)
parseAttributes (x@(y:ys):xs) n
    | isSpace y = parseAttributes (ys:xs) n
    | otherwise = case validateAttribute (trim $ takeWhile (/= '{') x) of
        Just att -> case get xs (n + 1) of
            (c, rm, n') -> case parseAttributes rm n' of
                Left zs -> Left ((att, c, n, n'):zs)
                e -> e
        Nothing -> Right (CDSLParseError {
            pErr = NotAnAttributeError
            , pExpr = Text (trim $ takeWhile (/= '{') x)
            , rawExpr = "'" ++ x ++ "'" }, n)
        where
            get :: [String] -> Int -> ([String], [String], Int)
            get [] i = ([], [], i)
            get (z:zs) i
                | '}' `elem` z = ([takeWhile (/= '}') z], zs, i + 1)
                | otherwise = case get zs (i + 1) of
                    (c, rm, i') -> (z:c, rm, i')


concatFeatures :: String -> [String]
concatFeatures xs = splitOn ";" (unwords (map trim (words xs)))

parseFeatures :: [String] -> Int -> Either [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] (CDSLParseError, Int)
parseFeatures [] _ = Left []
parseFeatures ("":xs) n = parseFeatures xs n
parseFeatures (x:xs) n = case readCDSL x of
    Left fe -> case parseFeatures xs (n + 1) of
        Left fs -> Left (fe:fs)
        e -> e
    Right (f, er) -> Right (CDSLParseError {pErr=OnValidateExpressions f er, pExpr=Null, rawExpr=x}, n)
