module LoadGD where
import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first, Bifunctor (second, bimap))
import Data.Either (partitionEithers)
import Feature (Feature (GameName, Saved), fromStringToFeature, validateKeyWords)
import CDSLExpr (CDSLExpr (Text, Null), CDSLParseError (MissingTerminationStatement, UnknownKeyWord, CDSLParseError, rawExpr, SyntaxError, pErr, pExpr, OnLoad))
import ParseCardDSL (parseCDSLFromString)
import GD (GameData)


allGames :: IO [String]
allGames = listDirectory gameFolder


loadFeatures :: GameData -> Int -> IO (Either GameData [CDSLParseError])
loadFeatures gd n = do
    g <- allGames
    if n < 0 || n >= length g
    then -- Shouldnt happen
        return (Right [CDSLParseError { pErr = SyntaxError, pExpr = Null, rawExpr = show n }])
    else
        do
            content <- readFile (gameFolder ++ (g !! n))
            case loadFeatures' gd (lines content) of
                Left gd' -> return (Left ((GameName, [Text (g !! n)]) : gd' ++ [(Saved, [Null])]))
                e -> return e


loadFeatures' :: GameData -> [String] -> Either GameData [CDSLParseError]
loadFeatures' fs c = case parseFileHelper c 1 of
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
            fixErrs ((f, e:ers):xs) = OnLoad f e : fixErrs ((f, ers):xs)
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
                Right (MissingTerminationStatement ("Missing termination statement around line " ++ show n ++ ", '" ++ x ++ "'"))
    _ -> Right (UnknownKeyWord ("Unknown gamerule at line " ++ show n ++ ", '" ++ x ++ "'"))
    where
        isEnd y = take 3 y == "END"
        onlyNothing [] = True
        onlyNothing (y:ys) = case y of
            Just _ -> False
            Nothing -> onlyNothing ys




mergeList :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeList [] ys = ys
mergeList ((xk, xv):xs) ys =
  case lookup xk ys of
    Just yv -> mergeList xs ((xk, yv):ys)
    Nothing -> mergeList xs ((xk, xv):ys)



lookupMany :: Eq a => [a] -> [(a, b)] -> [b]
lookupMany keys pairs = mapMaybe (`lookup` pairs) keys

lookupManyWithKey :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupManyWithKey keys list = [(k, v) | (k, v) <- list, k `elem` keys]

splitEithers :: [(a, [Either b c])] -> ([(a, [b])], [(a, [c])])
splitEithers [] = ([], [])
splitEithers ((a, x):xs) = bimap
  ((a, fst (partitionEithers x)) :)
  ((a, snd (partitionEithers x)) :) (splitEithers xs)


