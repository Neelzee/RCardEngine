module Terminal.ExecGameCommands where

import Data.List.Extra (trim, intercalate)
import CDSL.ParseCardDSL (validateCDSLExpression, fromCDSLToString)
import Terminal.GameCommands (Flag, GCEffect (GCEffect, se, gcErr, ve), GameCommand (GameCommand, Close, Test, Help, Clear, Edit, Copy, List), CommandEffect (CommandEffect, short, verbose), GCError (CDSLError, InvalidCommandArgumentError, MissingOrCorruptData), commands)
import GameData.GD (GameData)
import CDSL.CDSLExpr
import GameData.SaveGD (saveGameData)
import System.Console.ANSI (clearScreen)
import Functions (allGames, mergeList, lookupManyWithKey)
import Feature
import GameData.LoadGD
import Control.Monad (zipWithM)


execGameCommands :: GameData -> GameCommand -> IO GameData
execGameCommands gd c = case c of
    (Test f exprs flgs) -> do
        let r = zip (map validateCDSLExpression exprs) [0..]
        let (v, er) = splitRes r
        let gce = GCEffect {
                se = "The statements for feature '"
                ++ show f ++ "' had " ++ show (length v)
                ++ "/" ++ show (length r) ++ " valid expressions."

                , ve = "The statements for feature '"
                ++ show f ++ "' had " ++ show (length v)
                ++ "/" ++ show (length r) ++ " valid expressions.\n"
                ++ intercalate "\n" (map (\(e, i) -> "The expression at " ++ show i ++ " failed, giving the error " ++ show e) er)

                , gcErr = map (\(e, _) -> CDSLError (Left [e])) er
            }
        return gd
    (Close flgs) -> do
        let gce = GCEffect {
            se = "Closing game."
            , ve = if null gd
                then
                    "Closing game."
                else
                    if fst (last gd) == Saved
                        then
                            case lookup GameName gd of
                                Just [Text nm] -> "Closing game, saving " ++ nm
                                _ -> "Closing and saving game"
                        else
                            "Closing game, saving "
                            ++ show (length (break ((/= Saved) . fst) (reverse gd)))
                            ++ "/"
                            ++ show (length (break ((/= Saved) . fst) gd))
                            ++ " data"
            , gcErr = []
        }
        if "-f" `elem` flgs
            then
                undefined
            else
                do
                    res <- saveGameData gd
                    return []
    Help -> do
        printCommands commands
        return gd
    Clear -> do
        clearScreen
        return gd
    -- Edit command
    (Edit (Numeric gn) flgs) -> if null gd
        then
            do
                g <- allGames
                if gn `elem` [0..length g]
                    then
                        do
                            dl <- loadGameData gd gn
                            case dl of
                                Left gd' -> do
                                    let ecc = [GCEffect { se = "Loaded features from game " ++ (g !! gn), ve = "Loaded " ++ show (length gd') ++ " features from game " ++ (g !! gn), gcErr = [] }]
                                    execFlags ecc flgs
                                    return gd'
                                Right err -> do
                                        execFlags (map fromCDSLParseErrorOnLoad err) flgs
                                        return gd
                    else
                        do
                            execFlags [GCEffect { se = "Invalid number", ve = "Invalid number, expected a number between 0 and " ++ show (length g), gcErr = [InvalidCommandArgumentError ("Number " ++ show gn ++ " not within gange")] }] flgs
                            return gd
        else
            do
            putStrLn "Cannot load while other game is active"
            return gd
        -- copy
    (Copy (Numeric a) fet flgs) ->
        case gd of
                [] -> do
                    putStrLn "No game to copy too"
                    return gd
                _ -> do
                    g <- allGames
                    if a `elem` [0..(length g)]
                    then
                        do
                            res <- loadGameData [] a
                            case res of
                                Left gd' -> do
                                    let newFets = lookupManyWithKey fet gd'
                                    let gad = mergeList gd newFets
                                    let ecc = [GCEffect { se = "Copied " ++ show (length newFets) ++ " from " ++ (g !! a)
                                        , ve = intercalate "\n" (map (\(f, exprs) -> show f ++ " : " ++ intercalate "," (map fromCDSLToString exprs)) newFets)
                                        , gcErr = []} ]
                                    execFlags ecc flgs
                                    return gad
                                Right err -> do
                                    execFlags (map fromCDSLParseErrorOnLoad err) flgs
                                    return gd
                    else
                        do
                            putStrLn ("Expected a number between 0 and " ++ show (length g) ++ ", in 'from', but got " ++ show a ++ " instead")
                            return gd
    -- List games
    (List flgs) -> do
        ecc <- listGameData
        execFlags ecc flgs
        return gd
    e -> do
        putStrLn ("Command " ++ show e ++ " has no implemantation")
        return gd

    where
        splitRes :: [(Either a b, Int)] -> ([(a, Int)], [(b, Int)])
        splitRes [] = ([], [])
        splitRes ((x, i):xs) = case x of
            Left l -> case splitRes xs of
                (ls, rs) -> ((l, i):ls, rs)
            Right r -> case splitRes xs of
                (ls, rs) -> (ls, (r, i):rs)



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
                _ -> GCEffect { se = "Failed listing game: " ++ show n ++ ", found no name", ve = "Failed loading game: " ++ show n ++ ", found no name", gcErr = [MissingOrCorruptData ("Failed loading game: " ++ show n)]} : listGameData' xs (n + 1)
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