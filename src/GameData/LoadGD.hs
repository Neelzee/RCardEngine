module GameData.LoadGD (loadGameData, loadGameData') where

import System.Directory (listDirectory)
import Constants (gameFolder, gameExtension)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, Bifunctor (second))
import Feature (Feature (GameName, Saved, CardSuits, CardRanks, CardValues, CardEffects), fromStringToFeature, validateKeyWords, isAFeatureOf, Attribute (None), validateAttribute)
import CDSL.ParseCardDSL (processIfString, parseStringList, parseCDSLPlayerAction, readCDSL, validateFeature)
import GameData.GD (GameData)
import Functions (mergeList, removeMaybe, allGames, trim, lookupAll)
import CDSL.CDSLValidater (validateCDSLExpression)
import Data.Either (partitionEithers)
import Data.List (stripPrefix)
import Data.Char (isSpace)
import Data.List.Extra (split, splitOn)
import CDSL.CDSLExpr
import CardGame.CardFunctions


loadGameData :: GameData -> Int -> IO (Either GameData (CDSLParseError, Int))
loadGameData gd n = do
    g <- allGames
    let gm = reverse $ takeWhile (/= '.') (reverse (g !! n))
    if n < 0 || n >= length g
    then -- Shouldnt happen
        return (Right (CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show n }, 0))
    else
        do
            content <- readFile (gameFolder ++ "/" ++ (g !! n))
            case loadGameData' gd content of
                Left gamedata -> return (Left ((GameName, [Text (g !! n)]):gamedata))
                e -> return e


loadGameData' :: GameData -> String -> Either GameData (CDSLParseError, Int)
loadGameData' oldGD c = case parseFile c of
    Left nGd -> case validateFeatureAttributes nGd of
        Left gamedata -> case validateExpr gamedata of
            Left gd -> case validateGameData gamedata of
                Nothing -> Left (mergeList (concatMap snd gd) oldGD)
                Just (e, n) -> Right (e, n)
            Right e -> Right e
        Right e -> Right e
    Right e -> Right e

    where
        validateGameData :: [(Attribute, [(Feature, [CDSLExpr])], Int, Int)] -> Maybe (CDSLParseError, Int)
        validateGameData gd = case (lookup CardSuits (concatMap (\(_, a, _, _) -> a) gd)
            , lookup CardRanks (concatMap (\(_, a, _, _) -> a) gd)) of
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



        validateExpr :: [(Attribute, [(Feature, [CDSLExpr])], Int, Int)] ->
            Either [(Attribute, [(Feature, [CDSLExpr])])] (CDSLParseError, Int)
        validateExpr [] = Left []
        validateExpr (x@(att, fets, st, _):xs) = case parse fets st of
            Just e -> Right e
            Nothing -> case validateExpr xs of
                Left f -> Left ((att, fets):f)
                e -> e
            where
                parse :: [(Feature, [CDSLExpr])] -> Int -> Maybe (CDSLParseError, Int)
                parse [] _ = Nothing
                parse ((_, []):fs) n = parse fs n
                parse ((f, y:ys):fs) n = case validateCDSLExpression y of
                    Left _ -> parse ((f, ys):fs) (n + 1)
                    Right e -> Just (CDSLParseError { pErr = OnValidateFeature f e, pExpr = Null, rawExpr = "" }, n)

        validateFeatureAttributes :: [(Attribute, [(Feature, [CDSLExpr])], Int, Int)] ->
            Either [(Attribute, [(Feature, [CDSLExpr])], Int, Int)] (CDSLParseError, Int)
        validateFeatureAttributes [] = Left []
        validateFeatureAttributes (x@(a, fets, st, _):xs) = case parse a fets st of
            Just e -> Right e
            Nothing -> case validateFeatureAttributes xs of
                Left f -> Left (x:f)
                e -> e
            where
                parse :: Attribute -> [(Feature, [CDSLExpr])] -> Int -> Maybe (CDSLParseError, Int)
                parse _ [] _ = Nothing
                parse att ((f, _):fes) n = if f `isAFeatureOf` att
                    then
                        parse att fes (n + 1)
                    else
                        Just (CDSLParseError { pErr = NotAFeatureOfAttribute att f, pExpr = Null, rawExpr = "" }, n)


        -- TODO: Change in such a way that an error is shown on invalid feature
        {-
        let gd = removeMaybe (map (first fromStringToFeature) nGd)
        let (gd', errs) = readGD gd
        let gd'' = mergeList fs gd'
        let fx = map (second (map validateCDSLExpression)) gd''
        let (valid, vErrs) = checkFx fx
        if null errs
            then
                if null vErrs || all (null . snd) vErrs
                    then
                        Left gd''
                    else
                        Right (fixExcErrs vErrs)

            else
                Right (fixErrs errs)
    Right e -> Right [e]
    where
        fixErrs :: [(Feature, [CDSLParseError])] -> [CDSLParseError]
        fixErrs [] = []
        fixErrs ((_, []):xs) = fixErrs xs
        fixErrs ((f, e:ers):xs) = (e { pErr = OnLoad f (pErr e) }) : fixErrs ((f, ers):xs)

        fixExcErrs :: [(Feature, [CDSLExecError])] -> [CDSLParseError]
        fixExcErrs [] = []
        fixExcErrs ((_, []):xs) = fixExcErrs xs
        fixExcErrs ((f, errs):xs) = (CDSLParseError {
            pErr = OnValidateFeature f errs
            , pExpr = Null, rawExpr = "" }) : fixExcErrs xs

        checkFx :: [(Feature, [Either CDSLExpr [CDSLExecError]])] ->
            ([(Feature, [CDSLExpr])], [(Feature, [CDSLExecError])])
        checkFx [] = ([], [])
        checkFx ((f, x):xs) = case partitionEithers x of
            (l, r) -> case checkFx xs of
                (vl, il) -> ((f, l):vl, (f, concat r):il)
        -}


readGD :: [(Feature, [String])] -> ([(Feature, [CDSLExpr])], [(Feature, [CDSLParseError])])
readGD [] = ([], [])
readGD ((_, []):ys) = readGD ys
readGD ((f, x:xs):ys) = case readCDSL x of
    Left (f', ex) -> if True
        then
            case readGD ((f, xs):ys) of
                (fex, ferr) -> ((f', ex):fex, ferr)
        else
            case readGD ((f, xs):ys) of
                (fex, ferr) -> (fex, (f, [CDSLParseError {rawExpr=x, pExpr=Null, pErr=MissMatchFeatureError}]):ferr)
    Right (_, err) -> case readGD ((f, xs):ys) of
                (fex, ferr) -> (fex, (f, err):ferr)





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


parseFile :: String -> Either [(Attribute, [(Feature, [CDSLExpr])], Int, Int)] (CDSLParseError, Int)
parseFile xs = case parseAttributes (removeComments $ lines xs) 1 of
    Left ys -> parse ys
    Right e -> Right e
    where
        parse :: [(Attribute, [String], Int, Int)] ->
            Either [(Attribute, [(Feature, [CDSLExpr])], Int, Int)] (CDSLParseError, Int)
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
parseAttributes (x:xs) n = case validateAttribute (trim $ takeWhile (/= '{') x) of
    Just att -> case get xs (n + 1) of
        (c, rm, n') -> case parseAttributes rm n' of
            Left ys -> Left ((att, c, n, n'):ys)
            e -> e
    Nothing -> Right (CDSLParseError {
        pErr = NotAnAttributeError
        , pExpr = Text (trim $ takeWhile (/= '{') x)
        , rawExpr = "'" ++ x ++ "'" }, n)
    where
        get :: [String] -> Int -> ([String], [String], Int)
        get [] i = ([], [], i)
        get (y:ys) i
            | '}' `elem` y = ([takeWhile (/= '}') y], ys, i + 1)
            | otherwise = case get ys (i + 1) of
                (c, rm, i') -> (y:c, rm, i')


concatFeatures :: String -> [String]
concatFeatures xs = splitOn ";" (unwords (map trim (words xs)))

parseFeatures :: [String] -> Int -> Either [(Feature, [CDSLExpr])] (CDSLParseError, Int)
parseFeatures [] _ = Left []
parseFeatures ("":xs) n = parseFeatures xs n
parseFeatures (x:xs) n = case readCDSL x of
    Left fe -> case parseFeatures xs (n + 1) of
        Left fs -> Left (fe:fs)
        e -> e
    Right (f, er) -> Right (CDSLParseError {pErr=OnValidateExpressions f er, pExpr=Null, rawExpr=x}, n)



test :: String -> IO ()
test str = print (parseFile str)