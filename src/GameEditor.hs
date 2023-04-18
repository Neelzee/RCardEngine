module GameEditor where

import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Data.List (intercalate)
import Feature (Feature (Saved, GameName))
import GameData.GD (GameData)
import CDSL.CDSLExpr (CDSLExpr(Text, Null))
import CDSL.ParseCardDSL (fromCDSLToString, validateCDSLExpression)
import Terminal.GameCommands (GameCommand (Create, Add, Update, Remove, Status, Save, Test, Close), GCEffect (GCEffect, se, ve, gcErr), GCError (MissingFeatureError, GCError, errType, input, OpenGameDataError, NoGameDataError, CDSLError), showAll, Flag)
import Terminal.ValidateGameCommands (validateGameCommand)
import Terminal.ExecGameCommands (confirmCommand, printGCEffect)
import qualified Terminal.ExecGameCommands as ExecGameCommands (execGameCommands)
import GameData.SaveGD (saveGameData)

editor :: GameData -> IO ()
editor gd = do
    case lookup GameName gd of
        Just (Text nm:_) -> putStr ("edit -> " ++ nm ++ " > ")
        _ -> putStr "edit > "
    hFlush stdout
    c <- getLine
    case validateGameCommand c of
        Left cm -> case cm of
            (Close flg) -> do
                gce <-case (lookup Saved gd, lookup GameName gd) of
                    (Just _, Just [Text gm]) -> do
                        let (diff, _) = span ((/=Saved) . fst) gd
                        let gc = GCEffect { se = "Closing GameData " ++ gm, ve = "Thrashed a total of " ++ show (length diff - 1) ++ " features", gcErr = [] }
                        return [gc]
                    _ -> return [GCEffect { se = "Closing GameData", ve = "Trashed a total of " ++ show (length gd - 1) ++ " features", gcErr = [] }]
                res <- confirmCommand cm gce flg
                if res
                    then
                        return ()
                    else
                        editor gd
            _ -> do
                gd'<- execGameCommand cm gd
                editor gd'
        Right e -> do
            print e
            editor gd



-- Takes in user input
execGameCommand :: GameCommand -> GameData -> IO GameData
execGameCommand c gd = case c of
    -- Add feature command
    (Add fs s flg) -> if null gd
        then
            do
                let gce = [GCEffect { se = "No GameData too add feature too.", ve = "No GameData too add '" ++ show fs ++ "' too.",
                gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
                printGCEffect gce flg
                return gd
        else
            do
                let gce = [GCEffect { se = "Added " ++ show fs, ve = "Added feature " ++ show fs ++ ":\n" ++ intercalate "\n" (map fromCDSLToString s), gcErr = [] }]
                res <- confirmCommand c gce flg
                if res
                    then
                        do
                            return ((fs, s):gd)
                    else
                        do
                            return gd

    -- Test command
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
        res <- confirmCommand c [GCEffect { se = "Check validity of: " ++ intercalate "," (map show exprs), ve = "Check validity of: " ++ intercalate "\n\t" (map show exprs), gcErr = []}] flgs
        if res
            then
                do
                    printGCEffect [gce] flgs
                    return gd
            else
                return gd


    -- Update feature
    (Update fs s flg) -> if null gd
        then
            do
                let gce = [GCEffect { se = "No GameData too update feature.", ve = "No GameData too update feature '" ++ show fs ++ "'",
                gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
                printGCEffect gce flg
                return gd
        else
            case lookup fs gd of
                Just old -> do
                    let (gd', gc) = removeFeature gd [fs]
                    let gce = gc
                            ++[
                            GCEffect { se = "Updated " ++ show fs
                                , ve = "Updated " ++ show fs ++ " from\n" ++ intercalate "\n\t" (map fromCDSLToString old) ++ "\nto\n" ++ intercalate "\n\t" (map fromCDSLToString s)
                                , gcErr = []}]
                    res <- confirmCommand c gce flg
                    if res
                        then
                            return gd'
                        else
                            return gd

                Nothing -> do
                    let gce =[GCEffect { se = show fs ++ " not found in " ++ case lookup GameName gd of
                            Just [Text gm] -> gm ++ "."
                            _ -> "gamedata."
                        , ve = show fs ++ " not found in " ++ case lookup GameName gd of
                            Just [Text gm] -> gm ++ "."
                            _ -> "gamedata."
                        , gcErr = [GCError { errType = MissingFeatureError fs, input = showAll c }]}]
                    printGCEffect gce flg
                    return gd
    -- remove
    (Remove features flg) -> if null gd
        then
            do
                let gce = [GCEffect { se = "No GameData too remove from.", ve = "No GameData too remove '" ++ intercalate "," (map show features) ++ "' from.",
                gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
                printGCEffect gce flg
                return gd
        else
            do
                let (gd', gce) = removeFeature gd features
                res <- confirmCommand c gce flg
                if res
                    then
                        return gd'
                    else
                        return gd
    -- status
    (Status flg) -> if null gd
      then
        do
            let gce = [GCEffect { se = "No GameData too show status of.", ve = "No GameData too show status of.",
                gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
            printGCEffect gce flg
            return gd
      else
        do
            let gce = [GCEffect { se = "Show GameData status", ve = "Show GameData status", gcErr = [] }]
            res <- confirmCommand c gce flg
            if res
                then
                    do
                        printGCEffect (gameDataStatus gd) flg
                        return gd
                else
                    return gd
    -- save
    (Save flg) -> if null gd
        then
            do
                let gce = [GCEffect { se = "No GameData too save.", ve = "No GameData too save." ,
                gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
                printGCEffect gce flg
                return gd
        else
            do
                gce <- case lookup Saved gd of
                    Just _ -> do
                        let (_, diff) = span ((/=Saved) . fst) gd
                        let (_, ecc) = removeFeature gd [Saved]
                        let gc = GCEffect { se = "Saved game data", ve = "Saved a total of " ++ show (length diff - 1) ++ " new features", gcErr = [] }
                        return (gc:ecc)
                    Nothing -> return [GCEffect { se = "Saved game data", ve = "Saved a total of " ++ show (length gd - 1) ++ " features", gcErr = [] }]
                res <- confirmCommand c gce flg
                if res
                    then
                        do
                            let (gd', _) = removeFeature gd [Saved]
                            _ <- saveGameData gd
                            return ((Saved, [Null]) : gd')
                    else
                        return gd
    _ -> do
            ExecGameCommands.execGameCommands c
            return gd


splitRes :: [(Either a b, Int)] -> ([(a, Int)], [(b, Int)])
splitRes [] = ([], [])
splitRes ((x, i):xs) = case x of
    Left l -> case splitRes xs of
        (ls, rs) -> ((l, i):ls, rs)
    Right r -> case splitRes xs of
        (ls, rs) -> (ls, (r, i):rs)


removeFeature :: GameData -> [Feature] -> (GameData, [GCEffect])
removeFeature fs xs = removeFeature' fs xs []

removeFeature' :: GameData -> [Feature] -> [GCEffect] -> (GameData, [GCEffect])
removeFeature' gd [] ecc = (gd, ecc)
removeFeature' [] _ ecc = ([], ecc)
removeFeature' gd (f:xs) ecc = case lookup f gd of
        Just old -> do
            let gd' = rmFirst f gd
            removeFeature' gd' xs (GCEffect { se = "Removed: " ++ show f, ve = "Removed: " ++ show f ++ " : " ++ intercalate "," (map show old), gcErr = [] }:ecc)
        Nothing -> removeFeature' gd xs (GCEffect { se = "No instance of " ++ show f ++ " found.", ve = "No instance of " ++ show f ++ " found."
            , gcErr = [GCError { errType = MissingFeatureError f, input = show f }]}:ecc)
    where
        rmFirst :: Feature -> GameData -> GameData
        rmFirst _ [] = []
        rmFirst x (y@(z, _):ws)
            | x == z = ws
            | otherwise = y : rmFirst x ws



gameDataStatus :: GameData -> [GCEffect]
gameDataStatus [] = []
gameDataStatus ((Saved, _):xs) = gameDataStatus xs
gameDataStatus ((GameName, _):xs) = gameDataStatus xs
gameDataStatus ((f, s):xs) = GCEffect { se = show f, ve = "Feature " ++ show f ++ " : Statement ->\n\t" ++ intercalate "\n\t" (map fromCDSLToString s), gcErr = []}: gameDataStatus xs