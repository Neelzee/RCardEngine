module Terminal.ExecGameCommands where

import Data.List.Extra (trim, intercalate)
import CDSL.ParseCardDSL (fromCDSLToString)
import GameData.GD (GameData)
import CDSL.CDSLExpr
import GameData.SaveGD (saveGameData)
import System.Console.ANSI (clearScreen)
import Functions (allGames, mergeList, lookupManyWithKey)
import Feature
import GameData.LoadGD
import Control.Monad (zipWithM, when)
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
    agd <- zipWithM (\ _x i -> loadGameData [] i) games [0..]
    return (listGameData' agd 0)
    where
        listGameData' :: [Either GameData [CDSLParseError]] -> Int -> [GCEffect]
        listGameData' [] _ = []
        listGameData' (x:xs) n = case x of
            Left gd -> case lookup GameName gd of
                Just (Text gm:_) -> GCEffect { se = show n ++ " " ++ gm, ve = "Game:\n" ++ gm ++ "\nFeatures:\n" ++ intercalate "\n" (map ve (gameDataStatus gd)) ++ "\n" , gcErr = [] } : listGameData' xs (n + 1)
                _ -> GCEffect { se = "Failed listing game: " ++ show n ++ ", found no name", ve = "Failed loading game: " ++ show n ++ ", found no name", gcErr = [MissingOrCorruptDataError ("Failed loading game: " ++ show n)]} : listGameData' xs (n + 1)
            Right e -> GCEffect { se = "Failed listing game: " ++ show n, ve = "Failed loading game: " ++ show n ++ ", error: " ++ show e, gcErr = [CDSLError (Right e)] } : listGameData' xs (n + 1)


printCommands :: [GameCommand] -> IO ()
printCommands xs = printTable (pc xs)
    where
        pc [] = []
        pc ((GameCommand nm inf ex):ys) = (show nm, inf, ex) : pc ys
        pc (_:ys) = pc ys



fromCDSLParseErrorOnLoad :: CDSLParseError -> GCEffect
fromCDSLParseErrorOnLoad e = GCEffect { se = "Failed loading game", ve = "Failed loading game, error: " ++ show e, gcErr = [CDSLError (Right [e])] }

gameDataStatus :: GameData -> [GCEffect]
gameDataStatus [] = []
gameDataStatus ((Saved, _):xs) = gameDataStatus xs
gameDataStatus ((GameName, _):xs) = gameDataStatus xs
gameDataStatus ((f, s):xs) = GCEffect { se = show f, ve = "Feature " ++ show f ++ " : Statement ->\n\t" ++ intercalate "\n\t" (map fromCDSLToString s), gcErr = []}: gameDataStatus xs