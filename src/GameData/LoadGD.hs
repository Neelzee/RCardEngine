module GameData.LoadGD where

import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import Feature (Feature (GameName, Saved), fromStringToFeature, validateKeyWords, isAFeatureOf)
import CDSL.CDSLExpr (CDSLExpr (Text, Null), CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr), CDSLParseErrorCode (SyntaxError, OnLoad, MissingTerminationStatement, UnknownKeyWord, MissMatchFeatureError))
import CDSL.ParseCardDSL (parseCDSLFromString, parseIFCDSLFromString, parseCDSLFromStringList, processIfString, parseStringList, parseCDSLPlayerAction, readCDSL)
import GameData.GD (GameData)
import Functions (mergeList, removeMaybe, allGames)



loadGameData :: GameData -> Int -> IO (Either GameData [CDSLParseError])
loadGameData gd n = do
    g <- allGames
    let (_, gm) = break (=='.') (reverse (g !! n))
    let gm' = reverse (drop 1 gm)
    if n < 0 || n >= length g
    then -- Shouldnt happen
        return (Right [CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show n }])
    else
        do
            content <- readFile (gameFolder ++ "/" ++ (g !! n))
            case loadGameData' gd (lines content) of
                Left gd' -> return (Left ((GameName, [Text gm']) : gd' ++ [(Saved, [Null])]))
                e -> return e


loadGameData' :: GameData -> [String] -> Either GameData [CDSLParseError]
loadGameData' fs c = case parseFileHelper c 1 of
    Left nGd -> do
        let gd = removeMaybe (map (first fromStringToFeature) nGd)
        let (gd', errs) = readGD gd
        let gd'' = mergeList fs gd'
        if null errs
            then
                Left gd''
            else
                Right (fixErrs errs)
    Right e -> Right [e]
    where
        fixErrs [] = []
        fixErrs ((_, []):xs) = fixErrs xs
        fixErrs ((f, e:ers):xs) = (e { pErr = OnLoad f (pErr e) }) : fixErrs ((f, ers):xs)


readGD :: [(Feature, [String])] -> ([(Feature, [CDSLExpr])], [(Feature, [CDSLParseError])])
readGD [] = ([], [])
readGD ((_, []):ys) = readGD ys
readGD ((f, x:xs):ys) = case readCDSL x of
    Left (f', ex) -> if f' `isAFeatureOf` f
        then
            case readGD ((f, xs):ys) of
                (fex, ferr) -> ((f', ex):fex, ferr)
        else
            case readGD ((f, xs):ys) of
                (fex, ferr) -> (fex, (f, [CDSLParseError {rawExpr=x, pExpr=Null, pErr=MissMatchFeatureError}]):ferr)
    Right (_, err) -> case readGD ((f, xs):ys) of
                (fex, ferr) -> (fex, (f, err):ferr)





parseFileHelper :: [String] -> Int -> Either [(String, [String])] CDSLParseError
parseFileHelper [] _ = Left []
parseFileHelper (('#':_):xs) n = parseFileHelper xs (n + 1) -- Ignores comments
parseFileHelper ("":xs) n = parseFileHelper xs (n + 1) -- Ignores empty lines
parseFileHelper (x:xs) n = case validateKeyWords x of
    Just rule -> do
        let (stmt, rest) = break isEnd xs
        if onlyNothing (map validateKeyWords stmt)
            then -- Adding two, since we're skiping both the feature, and the end
                case parseFileHelper (drop 1 rest) (n + 2 + length stmt) of
                    Left rs -> Left ((rule, stmt):rs)
                    e -> e
            else
                Right (CDSLParseError { pErr = MissingTerminationStatement n, pExpr = Null, rawExpr = x })
    _ -> Right (CDSLParseError { pErr = UnknownKeyWord n, pExpr = Null, rawExpr = x })
    where
        isEnd y = take 3 y == "END"
        onlyNothing [] = True
        onlyNothing (y:ys) = case y of
            Just _ -> False
            Nothing -> onlyNothing ys


