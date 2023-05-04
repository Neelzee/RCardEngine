module Terminal.ExecGameCommands (
    execGameCommands
    , printGCEffect
    , confirmCommand
    , printTable
    , listGameData
    , printCommands
    , fromCDSLParseErrorOnLoad
    ) where

import Data.List.Extra (intercalate)
import CDSL.ExecCDSLExpr (fromCDSLToString)
import GameData.GD (GameData)
import CDSL.CDSLExpr
    ( infoCDSL, showF, CDSLExpr(Text), CDSLParseError )
import System.Console.ANSI (clearScreen)
import Functions (allGames, lookupM)
import Feature
    ( featureInfo, fromStringToFeature, Feature(GameName), Attribute (GameAttributes) )
import GameData.LoadGD ( loadGameData )
import Control.Monad (zipWithM, when)
import Terminal.GameCommands
    ( cmdInfo,
      commands,
      flags,
      showAll,
      Flag,
      GCEffect(..),
      GCError(CDSLError, MissingOrCorruptDataError),
      GameCommand(GameCommand, Help, Clear, List) )
import System.IO (hFlush, stdout)
import Terminal.ValidateGameCommands (validateGCFlags, validateGameCommand)
import CDSL.ParseCDSLExpr (parseOneCDSL)
import qualified Data.Map as Map (empty, lookup, toList)
import Data.Bifunctor (second)


execGameCommands :: GameCommand -> IO ()
execGameCommands c = case c of
    (Help Nothing) -> do
        printCommands commands
        return ()
    (Help (Just "flags")) -> putStrLn (intercalate "\n" (map (\(flg, inf) -> "Flag: " ++ unwords flg ++ ": " ++ inf) flags))
    (Help (Just "feature")) -> putStrLn (intercalate "\n" (map (\(fet, inf) -> "Feature: " ++ show fet ++ ": " ++ inf) featureInfo))
    (Help (Just "cdsl")) -> putStrLn (intercalate "\n" (map (\(ex, expl) -> "Expression: " ++ showF ex ++ ", " ++ expl) infoCDSL))
    (Help (Just xs)) -> case validateGCFlags (words xs) of
                Left flg -> case lookup flg flags of -- Give information about that flag
                    Just inf -> putStrLn ("Flag: " ++ unwords flg ++ ": " ++ inf)
                    Nothing -> putStrLn ("Found no information about the flag: '" ++ xs ++ "'")
                Right _ -> case validateGameCommand xs of
                    Left gc -> case lookup gc cmdInfo of -- Give information about that game command
                        Just (inf, exl) -> putStrLn ("Command: " ++ show gc ++ ": " ++ inf ++ ", example: " ++ exl)
                        Nothing -> putStrLn ("Found no information about the command: '" ++ xs ++ "'")
                    Right _ -> case fromStringToFeature xs of
                        Just fet -> case lookup fet featureInfo of
                            Just inf -> putStrLn ("Feature: " ++ show fet ++ ": " ++ inf)
                            Nothing -> putStrLn ("Found no information about the feature: '" ++ xs ++ "'")
                        Nothing -> case parseOneCDSL (xs : repeat " null") 0 of
                            Left (ex, _) -> case lookup ex infoCDSL of -- Give information about that expression
                                Just expl -> putStrLn ("Expression: " ++ showF ex ++ ", " ++ expl)
                                Nothing -> putStrLn ("Found no information about the CDSL expression: '" ++ showF ex ++ "'")
                            Right _ -> error ("INVALID HELP: '" ++ xs ++ "'") -- Should not happen, since this should be caught in the validateGameCommand function

    Clear -> do
        clearScreen
        return ()

    (List flg) -> do
        let ge = GCEffect { se = "List of all Games.", ve = "List of all Games.", gcErr = []}
        res <- confirmCommand c [ge] flg
        when res $ do
                    inf <- listGameData
                    printGCEffect inf flg
                    return ()
    _ -> do
        putStrLn ("No generic execuction for command '" ++ show c ++ "' found")
        return ()



printGCEffect :: [GCEffect] -> Flag -> IO ()
printGCEffect xs flg
    | "-verbose" `elem` flg = putStrLn (intercalate "\n" (map ve xs))
    | "-quiet" `elem` flg = return ()
    | otherwise = putStrLn (intercalate "\n" (map se xs))


confirmCommand :: GameCommand -> [GCEffect] -> Flag -> IO Bool
confirmCommand c xs flg
    | "-confirm" `elem` flg && "-quiet" `elem` flg = do
            printGCEffect xs flg
            putStr "Continue? (y/n, default = n)"
            hFlush stdout
            ans <- getLine
            case ans of
                "y" -> return True
                _ -> return False
    | "-confirm" `elem` flg = do
            putStrLn ("The following command '" ++ showAll c ++ "', will result in these effects:")
            printGCEffect xs flg
            putStr "Continue? (y/n, default = n)"
            hFlush stdout
            ans <- getLine
            case ans of
                "y" -> return True
                _ -> return False
    | otherwise = return True




printTable :: [(String, String, String)] -> IO ()
printTable rows = do
  let (col1, col2, col3) = unzip3 rows
      max1 = maximum (map length col1)
      max2 = maximum (map length col2)
      max3 = maximum (map length col3)
      formatRow (a, b, c) = intercalate "  " [pad max1 a, pad max2 b, pad max3 c]
      pad len str = str ++ replicate (len - length str) ' '
      table = map formatRow rows
      separator = replicate (length (head table)) '-'
  putStrLn separator
  mapM_ putStrLn table
  putStrLn separator



listGameData :: IO [GCEffect]
listGameData = do
    games <- allGames
    agd <- zipWithM (\ _x i -> loadGameData Map.empty i) games [0..]
    return (listGameData' agd 0 (map (reverse . drop 1 . dropWhile (/='.') . reverse) games))
    where
        listGameData' :: [Either GameData (CDSLParseError, Int)] -> Int -> [String] -> [GCEffect]
        listGameData' [] _ _ = []
        listGameData' (x:xs) n g = case x of
            Left gd -> case Map.lookup GameAttributes gd of
                Just att -> case lookupM GameName att of
                    Just (_, Text gm:_) -> GCEffect { se = show n ++ ": " ++ gm, ve = "Game:\n" ++ gm ++ "\nFeatures:\n" ++ intercalate "\n" (map ve (gameDataStatus gd)) ++ "\n" , gcErr = [] } : listGameData' xs (n + 1) g
                    _ -> GCEffect { se = "Failed listing game: " ++ (g !! n) ++ ", found no name", ve = "Failed loading game: " ++ (g !! n) ++ ", found no name", gcErr = [MissingOrCorruptDataError ("Failed loading game: " ++ (g !! n))]} : listGameData' xs (n + 1) g
                _ -> GCEffect { se = "Failed listing game: " ++ (g !! n) ++ ", found no name", ve = "Failed loading game: " ++ (g !! n) ++ ", found no name", gcErr = [MissingOrCorruptDataError ("Failed loading game: " ++ (g !! n))]} : listGameData' xs (n + 1) g
            Right (e, i) -> GCEffect { se = "Failed listing game: " ++ (g !! n), ve = "Failed loading game: " ++ (g !! n) ++ ", error at: " ++ show e ++ ", at line: " ++ show i, gcErr = [CDSLError (Right [e])] } : listGameData' xs (n + 1) g


printCommands :: [GameCommand] -> IO ()
printCommands xs = printTable (pc xs)
    where
        pc [] = []
        pc ((GameCommand nm inf ex):ys) = (show nm, inf, ex) : pc ys
        pc (_:ys) = pc ys



fromCDSLParseErrorOnLoad :: (CDSLParseError, Int) -> GCEffect
fromCDSLParseErrorOnLoad (e, i) = GCEffect { se = "Failed loading game", ve = "Failed loading game, error: " ++ show e ++ " on line: " ++ show i, gcErr = [CDSLError (Right [e])] }

gameDataStatus :: GameData -> [GCEffect]
gameDataStatus gd = map (gds . second Map.toList) (Map.toList gd)



gds :: (Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])]) -> GCEffect
gds (att, fs) = GCEffect {
    ve = show att ++ "\n" ++ intercalate "\n\t" (showVE fs)
    , se = "Attribute:" ++ "\n\t" ++ show att ++ "\n\t\tFeatures:" ++ intercalate "\n\t\t" (showSE fs)
    , gcErr = []
    }
    where
        showVE :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> [String]
        showVE [] = []
        showVE (((fet, _), _):xs) = show fet : showVE xs

        showSE :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> [String]
        showSE [] = []
        showSE (((fet, Nothing), ex):xs) = (show fet ++ "\n\t\t\tExpressions:" ++ intercalate "\n\t\t\t" (map fromCDSLToString ex)) : showVE xs
        showSE (((fet, Just arg), ex):xs) = (show fet ++ "\n\t\t\tArguments:" ++ intercalate "\n\t\t\t" (map fromCDSLToString arg) ++ "\n\t\t\tExpressions:" ++ intercalate "\n\t\t\t" (map fromCDSLToString ex)) : showVE xs