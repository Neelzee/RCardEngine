module GameData.LoadGD (loadGameData
    , loadGameData'
    , validateCards
    , concatFeatures
    , removeComments
    , validateFeatures
    ) where

import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Feature (Feature (GameName, CardSuits, CardRanks, CardConstraints, ExceptionConstraints, CardCompare, IgnoreConstraints, CEChangeCard, CESwapHand, CETakeFromHand, CEGiveCard, CEPassNext, CEDrawCard, CardValues), isAFeatureOf, validateAttribute, Attribute (GameAttributes, CardAttributes))
import CDSL.ParseCDSLExpr (readCDSL, isCDSLExprNumeric, toNumeric)
import GameData.GD (GameData)
import Functions (allGames, trim, lookupM)
import CDSL.CDSLValidater (validateCDSLExpression, isCardField, isComperator, isCard, getNonCards, getNonComperators, getNonCardFields, getNonNumerics)
import Data.List.Extra (splitOn, headDef)
import CDSL.CDSLExpr
    ( CDSLExpr(CEffect, Cards, Null, Text),
      CDSLParseError(..),
      CDSLParseErrorCode(OnValidateExpressions, SyntaxError,
                         OnValidateFeature, NotAFeatureOfAttribute, InvalidExpressionError,
                         InvalidFeatureArgumentError, MissMatchCardError,
                         NotAnAttributeError) )
import CardGame.CardFunctions
    ( cardElem,
      defaultCardRanks,
      defaultCardSuits,
      defaultCardValues,
      makeCards,
      makeDeck )
import Data.Bifunctor (second)
import qualified Data.Map as Map
import Data.Char (isSpace)
import CardGame.Card (Card)


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
        Left gamedata -> case map (\(_, ex, s, _) -> mapMaybe (`validateFeatures` s) ex) gamedata of
            err -> if all null err
                then
                    do

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
                            [] -> case validateExpr gamedata of
                                    Left gd -> case validateGameData gamedata of
                                        Nothing -> Left (Map.fromList (map (second Map.fromList) gd) `Map.union` oldGD)
                                        Just (e, n) -> Right (e, n)
                                    Right e -> Right e
                            er -> Right (headDef (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = "'" ++ show er ++ "'"}, 0) er)
                else
                    Right (headDef (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = "'" ++ show err ++ "'"}, 0) (concat err))
        Right e -> Right e
    Right e -> Right e

validateGameData :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])], Int, Int)] -> Maybe (CDSLParseError, Int)
validateGameData gd = case (lookup CardSuits (concatMap (\(_, a, _, _) -> map (\((x, _), y) -> (x, y)) a ) gd)
    , lookup CardRanks (concatMap (\(_, a, _, _) -> map (\((x, _), y) -> (x, y)) a ) gd)) of
    (Just suits, Just ranks) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) suits ranks
    (Just suits, Nothing) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) suits defaultCardRanks
    (Nothing, Just ranks) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) defaultCardSuits ranks
    (Nothing, Nothing) -> parse (map (\(_, d, e, f) -> (concatMap snd d, e, f)) gd) defaultCardSuits defaultCardRanks
    where
        parse :: [([CDSLExpr], Int, Int)] -> [CDSLExpr] -> [CDSLExpr] -> Maybe (CDSLParseError, Int)
        parse [] _ _= Nothing
        parse ((ys, st, _):xs) suits ranks = case mapMaybe (\a -> validate a suits ranks) ys of -- Not on exact line, but close enough
            [] -> parse xs suits ranks
            e -> Just (head e, st)


        validate :: CDSLExpr -> [CDSLExpr] -> [CDSLExpr] -> Maybe CDSLParseError
        validate ce@(CEffect _ crds) suits ranks = case filter (\crd -> not (cardElem crd (makeDeck suits ranks []))) crds of
            [] -> Nothing
            ex -> Just (CDSLParseError { pErr = MissMatchCardError ex (makeDeck suits ranks []), pExpr = ce, rawExpr = "" })
        validate _ _ _ = Nothing




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




validateFeatures :: ((Feature, Maybe [CDSLExpr]), [CDSLExpr]) -> Int -> Maybe (CDSLParseError, Int)
validateFeatures ((ExceptionConstraints, Just comp), ex) n = case (all isComperator comp, isCard (makeCards ex)) of
    (True, True) -> Nothing
    (False, _) ->  Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = headDef Null (mapMaybe getNonComperators comp), rawExpr = "" }, n)
    (_, False) -> Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((ExceptionConstraints, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CardConstraints, Nothing), ex) n = if all isCardField ex
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCardFields ex), rawExpr = "" }, n)
validateFeatures ((CardCompare, Nothing), ex) n = if all isComperator ex
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonComperators ex), rawExpr = "" }, n)
validateFeatures ((CardCompare, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((IgnoreConstraints, Nothing), ex) n = if isCard (makeCards ex)
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((IgnoreConstraints, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CEChangeCard, Just comp), ex) n = case (all isCardField comp, isCard (makeCards ex)) of
    (True, True) -> Nothing
    (False, _) ->  Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = headDef Null (mapMaybe getNonCardFields comp), rawExpr = "" }, n)
    (_, False) -> Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((CEChangeCard, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CESwapHand, Nothing), ex) n = if isCard (makeCards ex)
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((CESwapHand, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CETakeFromHand, Nothing), ex) n = if isCard (makeCards ex)
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((CETakeFromHand, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CEGiveCard, Nothing), ex) n = if isCard (makeCards ex)
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((CEGiveCard, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CEPassNext, Nothing), ex) n = if isCard (makeCards ex)
    then
        Nothing
    else
        Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((CEPassNext, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures ((CEDrawCard, Just nr), ex) n = case (all isCDSLExprNumeric nr, isCard (makeCards ex)) of
    (True, True) -> Nothing
    (False, _) ->  Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = headDef Null (mapMaybe getNonNumerics nr), rawExpr = "" }, n)
    (_, False) -> Just (CDSLParseError { pErr = InvalidExpressionError, pExpr = headDef Null (mapMaybe getNonCards ex), rawExpr = "" }, n)
validateFeatures ((CEDrawCard, _), _) n = Just (CDSLParseError { pErr = InvalidFeatureArgumentError, pExpr = Null, rawExpr = "" } , n)
validateFeatures _ _ = Nothing


validateCards :: CDSLExpr -> [Card] -> Int -> Maybe (CDSLParseError, Int)
validateCards e@(CEffect _ crds) dck n = if all (`cardElem` dck) crds
    then
        Nothing
    else
        Just (CDSLParseError { pErr = MissMatchCardError crds dck, pExpr = e, rawExpr = "" }, n)
validateCards e@(Cards crds) dck n = if all (`cardElem` dck) crds
    then
        Nothing
    else
        Just (CDSLParseError { pErr = MissMatchCardError crds dck, pExpr = e, rawExpr = "" }, n)
validateCards _ _ _ = Nothing


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
parseFeatures ("":xs) n = parseFeatures xs (n + 1)
parseFeatures (x:xs) n = case readCDSL x of
    Left fe -> case parseFeatures xs (n + 1) of
        Left fs -> Left (fe:fs)
        e -> e
    Right (f, er) -> Right (CDSLParseError {pErr=OnValidateExpressions f er, pExpr=Null, rawExpr=x}, n)



