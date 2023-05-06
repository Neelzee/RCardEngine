module GameEditor (editor, gameDataStatus) where

import Prelude hiding (lookup)
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Data.List (intercalate)
import Feature (Feature (GameName), Attribute (GameAttributes), getAttribute)
import GameData.GD (GameData)
import CDSL.CDSLExpr (CDSLExpr(Text))
import CDSL.ExecCDSLExpr (fromCDSLToString)
import Terminal.GameCommands (GameCommand (Add, Update, Remove, Status, Save, Test, Close), GCEffect (GCEffect, se, ve, gcErr), GCError (MissingFeatureError, GCError, errType, input, NoGameDataError, CDSLError), showAll)
import Terminal.ValidateGameCommands (validateGameCommand)
import Terminal.ExecGameCommands (confirmCommand, printGCEffect)
import qualified Terminal.ExecGameCommands as ExecGameCommands (execGameCommands)
import GameData.SaveGD (saveGameData)
import CDSL.CDSLValidater (validateCDSLExpression)
import Data.Map (lookup, insert, fromList, delete, toList)
import Functions (lookupM)
import System.Directory (getAccessTime)
import Data.Bifunctor (second)

editor :: GameData -> IO ()
editor gd = do
    case lookup GameAttributes gd of
        Just ga -> case lookup (GameName, Nothing) ga of
            Just (Text nm:_) -> putStr ("edit -> " ++ nm ++ " > ")
            _ -> putStr "edit > "
        _ -> putStr "edit > "
    hFlush stdout
    c <- getLine
    case validateGameCommand c of
        Left cm -> case cm of
            (Close flg) -> do
                res <- confirmCommand cm [GCEffect { se = "Closing GameData", ve = "Closing GameData", gcErr = []}] flg
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
            case lookup (getAttribute fs) gd of
                Just att -> case lookupM fs att of
                    Just (k, vl) -> do
                        let gce = [GCEffect { se = "Added new content too feature.", ve = "Added: " ++ intercalate "\n\t" (map fromCDSLToString s), gcErr = []}]
                                ++ [GCEffect { se = "", ve = "Pre-existing: " ++ intercalate "\n\t" (map fromCDSLToString vl), gcErr = []}]
                                ++ [GCEffect { se = "", ve = "New: " ++ intercalate "\n\t" (map fromCDSLToString s), gcErr = []}]
                        res <- confirmCommand c gce flg
                        if res
                            then
                                do
                                    let att' = insert k (vl ++ s) att
                                    let gd' = insert (getAttribute fs) att' gd
                                    return gd'
                            else
                                return gd
                    Nothing -> do
                        let gce = [GCEffect { se = "Added " ++ show fs, ve = "Added feature " ++ show fs ++ ":\n" ++ intercalate "\n" (map fromCDSLToString s), gcErr = [] }]
                        res <- confirmCommand c gce flg
                        if res
                            then
                                do
                                    let att' = insert (fs, Nothing) s att
                                    let gd' = insert (getAttribute fs) att' gd
                                    return gd'
                            else
                                return gd

                Nothing -> do
                        let gce = [GCEffect { se = "Added new feature: " ++ show fs ++ ", which is part of a new attribute: " ++ show (getAttribute fs), ve = "Added feature " ++ show fs ++ ":\n" ++ intercalate "\n" (map fromCDSLToString s), gcErr = [] }]
                        res <- confirmCommand c gce flg
                        if res
                            then
                                do
                                    let gd' = insert (getAttribute fs) (fromList [((fs, Nothing), s)]) gd
                                    return gd'
                            else
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

                , gcErr = map (\(e, _) -> CDSLError (Left e)) er
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
            case lookup (getAttribute fs) gd of
                Just att -> case lookupM fs att of
                    Just (_, old) -> do
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
                        let gce = [GCEffect { se = "No GameData too update feature.", ve = "No GameData too update feature '" ++ show fs ++ "'",
                        gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
                        printGCEffect gce flg
                        return gd
                Nothing -> do
                    let gce = [GCEffect { se = "No GameData too update feature.", ve = "No GameData too update feature '" ++ show fs ++ "'",
                    gcErr = [GCError { errType = NoGameDataError, input = showAll c }]}]
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
                let gce = [GCEffect { se = "Saving GameData", ve = "Saving GameData", gcErr = [] }]
                res <- confirmCommand c gce flg
                if res
                    then
                        do
                            _ <- saveGameData gd
                            return gd
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
removeFeature' gd (f:xs) ecc = case lookup (getAttribute f) gd of
        Just att -> case lookupM f att of
            Just (k, old) -> do
                let gd' = rmFirst (getAttribute f) k gd
                removeFeature' gd' xs (GCEffect { se = "Removed: " ++ show f, ve = "Removed: " ++ show f ++ " : " ++ intercalate "," (map show old), gcErr = [] }:ecc)
            Nothing -> removeFeature' gd xs (GCEffect { se = "No instance of " ++ show f ++ " found.", ve = "No instance of " ++ show f ++ " found."
                , gcErr = [GCError { errType = MissingFeatureError f, input = show f }]}:ecc)
        Nothing -> removeFeature' gd xs (GCEffect { se = "No instance of " ++ show f ++ " found.", ve = "No instance of " ++ show f ++ " found."
            , gcErr = [GCError { errType = MissingFeatureError f, input = show f }]}:ecc)
    where
        rmFirst :: Attribute -> (Feature, Maybe [CDSLExpr]) -> GameData -> GameData
        rmFirst x y mp = case lookup x mp of
            Just mp' -> insert x (delete y mp') mp
            Nothing -> mp



gameDataStatus :: GameData -> [GCEffect]
gameDataStatus mp = gameDataStatus' (map (second toList) (toList mp))

gameDataStatus' :: [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])])] -> [GCEffect]
gameDataStatus' [] = []
gameDataStatus' ((at, fs):xs)
    = GCEffect {
        se = show at ++ "\n\t" ++ intercalate "\n\t" (map (\(f, _) -> show f) fs)
        , ve = show at ++ "\n\tFeatures:" ++ intercalate "\n\t\t" (map pShow fs)
        , gcErr = []
    } : gameDataStatus' xs
    where
        pShow :: ((Feature, Maybe [CDSLExpr]), [CDSLExpr]) -> String
        pShow ((fet, args), exprs) = case args of
            Just arg -> show fet ++ ", with arguments: " ++ intercalate ", " (map fromCDSLToString arg) ++ " ->\n\t\t\t" ++ intercalate "\n\t\t\t" (map fromCDSLToString exprs)
            Nothing -> show fet ++ " ->\n\t\t\t" ++ intercalate "\n\t\t\t" (map fromCDSLToString exprs)