module LoadGD where

import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, Bifunctor (second, bimap))
import Data.Either (partitionEithers)
import Feature (Feature (GameName, Saved), fromStringToFeature, validateKeyWords)
import CDSLExpr (CDSLExpr (Text, Null), CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr), CDSLParseErrorCode (SyntaxError, OnLoad, MissingTerminationStatement, UnknownKeyWord), CDSLExecError (CDSLExecError))
import ParseCardDSL (parseCDSLFromString, parseIFCDSLFromString, parseCDSLF)
import GD (GameData)
import Functions (splitEithers, mergeList)


allGames :: IO [String]
allGames = listDirectory gameFolder


main :: IO ()
main = do
    rawC <- readFile "test/CDSLExprTest/CardDSL.cdsl"
    let c = lines rawC
    print "\nTEST\n"
    res <- loadGameData'' [] c
    print res

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
        let (gd', errs) = partitionEithers (map (uncurry readGD) gd)
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
    where
        readGD :: Feature -> [String] -> Either (Feature, [CDSLExpr]) (Feature, [CDSLParseError])
        readGD f [] = Left (f, [])
        readGD f [x] = case parseCDSLF x of
            Left ex -> Left (f, ex)
            Right err -> Right (f, err)
        readGD f (x:xs) = case parseCDSLF x of
            Left ex -> case readGD f xs of
                Left (_, exs) -> Left (f, ex ++ exs)
                Right e -> Right e
            Right e -> Right (f, e)


loadGameData'' :: GameData -> [String] -> IO (Either GameData [CDSLParseError])
loadGameData'' fs c = case parseFileHelper c 1 of
    Left nGd -> do
        putStrLn "YEE"
        case parseCDSLF "Hearts, Diamonds, Clubs, Spades" of
            Left e -> do
                print "AAA"
                print e
            Right e -> print e
        print (length (parseCDSLF "Hearts, Diamonds, Clubs, Spades"))
        putStrLn "\n"
        let gd = removeMaybe (map (first fromStringToFeature) nGd)
        let (gd', errs) =  splitEithers (map (second (map parseCDSLF)) gd)
        print gd'
        putStrLn "\n\n\n"
        print errs
        let gd'' = mergeList fs (map (second concat) gd')
        if null errs
            then
                return (Left gd'')
            else
                return (Right (fixErrs (map (second concat) errs)))
        where
            removeMaybe [] = []
            removeMaybe ((x, y):xs) = case x of
                Just z -> (z, y) : removeMaybe xs
                Nothing -> removeMaybe xs
            fixErrs [] = []
            fixErrs ((_, []):xs) = fixErrs xs
            fixErrs ((f, e:ers):xs) = (e { pErr = OnLoad f (pErr e) }) : fixErrs ((f, ers):xs)
    Right e -> return (Right [e])



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


