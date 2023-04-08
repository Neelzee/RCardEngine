module Terminal.ExecGameCommands where

import Data.List.Extra (trim, intercalate)
import CDSL.ParseCardDSL (validateCDSLExpression, fromCDSLToString)
import GameData.GD (GameData)
import CDSL.CDSLExpr
import GameData.SaveGD (saveGameData)
import System.Console.ANSI (clearScreen)
import Functions (allGames, mergeList, lookupManyWithKey)
import Feature
import GameData.LoadGD
import Control.Monad (zipWithM)
import Terminal.GameCommands
import System.IO (hFlush, stdout)


execGameCommands :: GameCommand -> IO ()
execGameCommands c = case c of
    Help -> do
        printCommands commands
        return ()

    Clear -> do
        clearScreen
        return ()
    
    _ -> do
        putStrLn "Command not added"
        return ()



printGCEffect :: [GCEffect] -> Flag -> IO ()
printGCEffect xs flg
    | "-verbose" `elem` flg = putStrLn (intercalate "\n" (map ve xs))
    | otherwise = putStrLn (intercalate "\n" (map se xs))


confirmCommand :: GameCommand -> [GCEffect] -> Flag -> IO Bool
confirmCommand c xs flg = if "-confirm" `notElem` flg
    then
        return True
    else
        do
            putStrLn ("The following command '" ++ showAll c ++ "', will result in these effects:")
            printGCEffect xs flg
            putStr "Continue? (y/n, default = n)"
            hFlush stdout
            ans <- getLine
            case ans of
                "y" -> return True
                _ -> return False



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
    agd <- zipWithM (\ _x i -> loadGameData [] i) games [0..]
    return (listGameData' agd 0)
    where
        listGameData' :: [Either GameData [CDSLParseError]] -> Int -> [GCEffect]
        listGameData' [] _ = []
        listGameData' (x:xs) n = case x of
            Left gd -> case lookup GameName gd of
                Just (Text gm:_) -> GCEffect { se = gm, ve = "Game:\n" ++ gm ++ "\nFeatures:\n" ++ intercalate "\n" (map verbose (gameDataStatus gd)) ++ "\n" , gcErr = [] } : listGameData' xs (n + 1)
                _ -> GCEffect { se = "Failed listing game: " ++ show n ++ ", found no name", ve = "Failed loading game: " ++ show n ++ ", found no name", gcErr = [MissingOrCorruptDataError ("Failed loading game: " ++ show n)]} : listGameData' xs (n + 1)
            Right e -> GCEffect { se = "Failed listing game: " ++ show n, ve = "Failed loading game: " ++ show n ++ ", error: " ++ show e, gcErr = [CDSLError (Right e)] } : listGameData' xs (n + 1)


printCommands :: [GameCommand] -> IO ()
printCommands xs = printTable (pc xs)
    where
        pc [] = []
        pc ((GameCommand nm inf ex):ys) = (show nm, inf, ex) : pc ys
        pc (_:ys) = pc ys


execFlags :: [GCEffect] -> Flag -> IO ()
execFlags xs flg
    | "-e" `elem` flg && "-verbose" `elem` flg = putStrLn (intercalate "\n" (map ve xs))
    | "-e" `elem` flg = putStrLn (intercalate "\n" (map se xs))
    | otherwise = return ()


fromCDSLParseErrorOnLoad :: CDSLParseError -> GCEffect
fromCDSLParseErrorOnLoad e = GCEffect { se = "Failed loading game", ve = "Failed loading game, error: " ++ show e, gcErr = [CDSLError (Right [e])] }

gameDataStatus :: GameData -> [CommandEffect]
gameDataStatus [] = []
gameDataStatus ((Saved, _):xs) = gameDataStatus xs
gameDataStatus ((GameName, _):xs) = gameDataStatus xs
gameDataStatus ((f, s):xs) = CommandEffect { short = show f, verbose = "Feature " ++ show f ++ " : Statement ->\n" ++ intercalate "\n\t" (map fromCDSLToString s)}: gameDataStatus xs