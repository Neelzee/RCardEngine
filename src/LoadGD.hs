module LoadGD where
import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, Bifunctor (second, bimap))
import Data.Either (partitionEithers)
import Feature (Feature (GameName, Saved), fromStringToFeature, validateKeyWords)
import CDSLExpr (CDSLExpr (Text, Null), CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr), CDSLParseErrorCode (SyntaxError, OnLoad, MissingTerminationStatement, UnknownKeyWord))
import ParseCardDSL (parseCDSLFromString)
import GD (GameData)
import Functions (splitEithers, mergeList)


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
        let (gd', errs) = splitEithers (map (second (map (parseCDSLFromString . words))) gd)
        let gd'' = mergeList fs gd'
        if null errs
            then
                Left gd''
        else
            Right (fixErrs errs)
        where
            removeMaybe [] = []
            removeMaybe ((x, y):xs) = case x of
                Just z -> (z, y) : removeMaybe xs
                Nothing -> removeMaybe xs
            fixErrs [] = []
            fixErrs ((_, []):xs) = fixErrs xs
            fixErrs ((f, e:ers):xs) = (e { pErr = OnLoad f (pErr e) }) : fixErrs ((f, ers):xs)
    Right e -> Right [e]



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


