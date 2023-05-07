module GameData.DebugGD (loadGameDataDebug) where

import GameData.GD (GameData)
import CDSL.CDSLExpr (CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr), CDSLExpr (Text, CEffect, Null), CDSLParseErrorCode (..))
import qualified Data.Map as Map
import Functions (allGames, trim, lookupM)
import Constants (gameFolder)
import Feature (Attribute(..), Feature (..), validateAttribute, isAFeatureOf)
import CardGame.CardFunctions (makeDeck, cardElem, defaultCardSuits, defaultCardRanks, defaultCardValues)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (second)
import CDSL.ParseCDSLExpr (toNumeric, readCDSL)
import GameData.LoadGD (validateCards, concatFeatures, removeComments, validateFeatures)
import Data.Char (isSpace)
import CDSL.CDSLValidater (validateCDSLExpression)


loadGameDataDebug :: Int -> IO (GameData, [(CDSLParseError, Int)])
loadGameDataDebug n = do
    g <- allGames
    let gm = takeWhile (/= '.') (g !! n)
    if n < 0 || n >= length g
    then -- Shouldnt happen
        error "this should not happen, due to preveious argument validations"
    else
        do
            content <- readFile (gameFolder ++ "/" ++ (g !! n))
            case loadGameDataDebug' content of
                (gamedata, errs) -> do
                    case Map.lookup GameAttributes gamedata of
                        Just att -> return (Map.insert GameAttributes (Map.insert (GameName, Nothing) [Text gm] att) gamedata, errs)
                        Nothing -> return (Map.insert GameAttributes (Map.fromList [((GameName, Nothing), [Text gm])]) gamedata, errs)


loadGameDataDebug' :: String -> (GameData, [(CDSLParseError, Int)])
loadGameDataDebug' c = case parseFileDebug c of
    (nGd, nGdErrs) -> case validateFeatureAttributesDebug nGd of
        (gamedata, gamedataErrs) -> case map (\(_, ex, s, _) -> mapMaybe (`validateFeatures` s) ex) gamedata of
            err ->  do
                    (cs, cr, cv) <- case Map.lookup CardAttributes (Map.fromList (map (second Map.fromList . (\(a, ex, _, _) -> (a, ex))) gamedata)) of
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
                    let deck = makeDeck cs cr cv

                    case concatMap (\(_, fs, s, _) -> concatMap (\(_, e) -> mapMaybe (\x -> validateCards x deck s) e) fs) gamedata of
                        er -> case validateExprDebug gamedata of
                                (gd, gdErrs) -> case validateGameDataDebug gamedata of
                                    lErrs -> (Map.fromList (map (second Map.fromList) gd), nGdErrs ++ gamedataErrs ++ concat err ++ er ++ gdErrs ++ lErrs)




validateGameDataDebug :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] -> [(CDSLParseError, Int)]
validateGameDataDebug gd = case (lookup CardSuits (concatMap (\(_, a, _, _) -> map (\((x, _), y) -> (x, y)) a ) gd)
    , lookup CardRanks (concatMap (\(_, a, _, _) -> map (\((x, _), y) -> (x, y)) a ) gd)) of
    (Just suits, Just ranks) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) suits ranks
    (Just suits, Nothing) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) suits defaultCardRanks
    (Nothing, Just ranks) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) defaultCardSuits ranks
    (Nothing, Nothing) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) defaultCardSuits defaultCardRanks
    where
        parse :: [([CDSLExpr], Int, Int)] -> [CDSLExpr] -> [CDSLExpr] -> [(CDSLParseError, Int)]
        parse [] _ _= []
        parse ((ys, st, _):xs) suits ranks = case mapMaybe (\a -> validate a suits ranks) ys of -- Not on exact line, but close enough
            [] -> parse xs suits ranks
            e -> (head e, st) : parse xs suits ranks


        validate :: CDSLExpr -> [CDSLExpr] -> [CDSLExpr] -> Maybe CDSLParseError
        validate ce@(CEffect _ crds) suits ranks = case filter (\crd -> not (cardElem crd (makeDeck suits ranks []))) crds of
            [] -> Nothing
            ex -> Just (CDSLParseError { pErr = MissMatchCardError ex (makeDeck suits ranks []), pExpr = ce, rawExpr = "" })
        validate _ _ _ = Nothing




validateExprDebug :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] ->
    ([(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])])], [(CDSLParseError, Int)])
validateExprDebug [] = ([], [])
validateExprDebug ((att, fets, st, _):xs) = case parse fets st of
    errs -> case validateExprDebug xs of
        (fs, ers) -> ((att, fets):fs, errs ++ ers)
    where
        parse :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> Int -> [(CDSLParseError, Int)]
        parse [] _ = []
        parse ((_, []):fs) n = parse fs n
        parse ((f, y:ys):fs) n = case validateCDSLExpression y of
            Left _ -> parse ((f, ys):fs) (n + 1)
            Right e -> (CDSLParseError { pErr = OnValidateFeature (fst f) e, pExpr = Null, rawExpr = "" }, n) : parse ((f, ys):fs) (n + 1)




validateFeatureAttributesDebug :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] ->
    ([(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)], [(CDSLParseError, Int)])
validateFeatureAttributesDebug [] = ([], [])
validateFeatureAttributesDebug (x@(a, fets, st, _):xs) = case parse a fets st of
    Just e -> case validateFeatureAttributesDebug xs of
        (ats, ers) -> (ats, e:ers)
    Nothing -> case validateFeatureAttributesDebug xs of
        (ats, ers) -> (x:ats, ers)
    where
        parse :: Attribute -> [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> Int -> Maybe (CDSLParseError, Int)
        parse _ [] _ = Nothing
        parse att (((f, _), _):fes) n = if f `isAFeatureOf` att
            then
                parse att fes (n + 1)
            else
                Just (CDSLParseError { pErr = NotAFeatureOfAttribute att f, pExpr = Null, rawExpr = "" }, n)




parseFileDebug :: String -> ([(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)], [(CDSLParseError, Int)])
parseFileDebug xs = case parseAttributesDebug (removeComments $ lines xs) 1 of
    (ats, errs) -> case parse ats of
        (ats', errs') -> (ats', errs ++ errs')
    where
        parse :: [(Attribute, [String], Int, Int)] ->
            ([(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)], [(CDSLParseError, Int)])
        parse [] = ([], [])
        parse ((at, ys, st, ed):zs) = case parseFeaturesDebug (concatFeatures (unlines ys)) st of
            (fs, ers) -> case parse zs of
                (fs', ers') -> ((at, fs, st, ed):fs', ers ++ ers')





parseAttributesDebug :: [String] -> Int ->
    ([(Attribute, [String], Int, Int)], [(CDSLParseError, Int)])
parseAttributesDebug [] _ = ([], [])
parseAttributesDebug ("":xs) n = parseAttributesDebug xs (n + 1)
parseAttributesDebug (x@(y:ys):xs) n
    | isSpace y = parseAttributesDebug (ys:xs) n
    | otherwise = case validateAttribute (trim $ takeWhile (/= '{') x) of
        Just att -> case get xs (n + 1) of
            (c, rm, n') -> case parseAttributesDebug rm n' of
                (ats, errs) -> ((att, c, n, n'):ats, errs)
        Nothing -> case parseAttributesDebug xs (n + 1) of
            (ats, errs) -> (ats, (CDSLParseError {
                pErr = NotAnAttributeError
                , pExpr = Text (trim $ takeWhile (/= '{') x)
                , rawExpr = "'" ++ x ++ "'" }, n):errs)
        where
            get :: [String] -> Int -> ([String], [String], Int)
            get [] i = ([], [], i)
            get (z:zs) i
                | '}' `elem` z = ([takeWhile (/= '}') z], zs, i + 1)
                | otherwise = case get zs (i + 1) of
                    (c, rm, i') -> (z:c, rm, i')


parseFeaturesDebug :: [String] -> Int -> ([((Feature, Maybe [CDSLExpr]), [CDSLExpr])], [(CDSLParseError, Int)])
parseFeaturesDebug [] _ = ([], [])
parseFeaturesDebug ("":xs) n = parseFeaturesDebug xs (n + 1)
parseFeaturesDebug (x:xs) n = case readCDSL x of
    Left fe -> case parseFeaturesDebug xs (n + 1) of
        (fs, errs) -> (fe:fs, errs)
    Right (f, er) -> case parseFeaturesDebug xs (n + 1) of
        (fs, errs) -> (fs, (CDSLParseError {pErr=OnValidateExpressions f er, pExpr=Null, rawExpr=x}, n):errs)

