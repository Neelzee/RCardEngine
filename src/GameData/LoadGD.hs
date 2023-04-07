module GameData.LoadGD where

import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import Feature (Feature (GameName, Saved), fromStringToFeature, validateKeyWords)
import CDSL.CDSLExpr (CDSLExpr (Text, Null), CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr), CDSLParseErrorCode (SyntaxError, OnLoad, MissingTerminationStatement, UnknownKeyWord))
import CDSL.ParseCardDSL (parseCDSLFromString, parseIFCDSLFromString, parseCDSLFromStringList, processIfString, parseStringList, parseCDSLPlayerAction)
import GameData.GD (GameData)
import Functions (mergeList, removeMaybe)


allGames :: IO [String]
allGames = listDirectory gameFolder


loadGameData :: GameData -> Int -> IO (Either GameData [CDSLParseError])
loadGameData gd n = do
    g <- allGames
    if n < 0 || n >= length g
    then -- Shouldnt happen
        return (Right [CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show n }])
    else
        do
            content <- readFile (gameFolder ++ (g !! n))
            case loadGameData' gd (lines content) of
                Left gd' -> return (Left ((GameName, [Text (g !! n)]) : gd' ++ [(Saved, [Null])]))
                e -> return e


loadGameData' :: GameData -> [String] -> Either GameData [CDSLParseError]
loadGameData' fs c = case parseFileHelper c 1 of
    Left nGd -> do
        let gd = removeMaybe (map (first fromStringToFeature) nGd)
        let (gd', errs) = (mapMaybe readGDF gd, mapMaybe readGDE gd)
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

readGDF :: (Feature, [String]) -> Maybe (Feature, [CDSLExpr])
readGDF (f, []) = Just (f, [])
-- Checks if its a normal expr
readGDF (f, x:xs) = case parseCDSLFromString (words x) of
    Left ex -> case readGDF (f, xs) of
        Just (_, exs) -> Just (f, ex:exs)
        Nothing -> Just (f, [ex])
    Right _ -> case parseIFCDSLFromString x of
        Left ex -> case readGDF (f, xs) of
            Just (_, exs) -> Just (f, ex:exs)
            Nothing -> Just (f, [ex])
        Right _ -> case parseCDSLFromStringList x of
            Left ex -> case readGDF (f, xs) of
                Just (_, exs) -> Just (f, ex ++ exs)
                Nothing -> Just (f, ex)
            Right _ -> case parseCDSLPlayerAction x of
                Left ex -> case readGDF (f, xs) of
                    Just (_, exs) -> Just (f, ex ++ exs)
                    Nothing -> Just (f, ex)
                Right _ -> Just (f, parseStringList x)


readGDE :: (Feature, [String]) -> Maybe (Feature, [CDSLParseError])
readGDE (_, []) = Nothing
readGDE (f, x:xs) = case (parseIFCDSLFromString x, parseCDSLFromStringList x, parseCDSLFromString (words x)) of
    (Left _, _, _) -> readGDE (f, xs)
    (_, Left _, _) -> readGDE (f, xs)
    (_, _, Left _) -> readGDE (f, xs)
    (Right e, _, _) -> case readGDE (f, xs) of
        Just (_, err) -> Just (f, (e { pErr = OnLoad f (pErr e) }):err)
        Nothing -> Nothing






parseFileHelper :: [String] -> Int -> Either [(String, [String])] CDSLParseError
parseFileHelper [] _ = Left []
parseFileHelper (('#':_):xs) n = parseFileHelper xs (n + 1) -- Ignores comments
parseFileHelper ("":xs) n = parseFileHelper xs (n + 1)
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


