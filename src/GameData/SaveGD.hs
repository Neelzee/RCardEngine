module GameData.SaveGD where

import GameData.GD (GameData)
import Feature (Feature(..))
import System.Directory (listDirectory)
import Constants (gameFolder, gameExtension)
import Data.List (elemIndex)
import CDSL.CDSLExpr (CDSLExpr(..))
import System.IO (withFile, IOMode (WriteMode), hPrint, hPutStrLn)
import CDSL.ParseCardDSL (fromCDSLToString)
import GameData.LoadGD (loadGameData)
import Terminal.GameCommands


saveGameData :: GameData -> IO GCEffect
saveGameData gd = do
    let (_, gd') = span ((/=Saved) . fst) (reverse gd)
    saveGameData' gd'

saveGameData' :: GameData -> IO GCEffect
saveGameData' gd = do
    gl <- listDirectory gameFolder
    case lookup GameName gd of
        Just (Text gm:_) -> case elemIndex gm gl of
            Just i -> do
                res <- loadGameData [] i
                case res of
                    Left oldGd -> do
                        let new = findNew oldGd gd
                        let diff = findDiff oldGd gd
                        let sim = findSim oldGd gd
                        saveGD gd gm
                        return (GCEffect { se = "Overwrote old instance of " ++ gm,
                        ve = "Overwrote " ++ show (length diff) ++ " features, added " ++ show (length new) ++ " features, " ++ show (length sim) ++ " features unchanged"})
                    Right e -> do
                        let gc = (GCEffect { se = "Overwrote old instance of " ++ gm
                            , ve = "Overwrote old instance, cannot compare differences due to an error on load."
                            , gcErr = [CDSLError (Right e)]})
                        saveGD gd gm
                        return gc
            Nothing -> do
                saveGD gd gm
                let new = findNew [] gd
                return (GCEffect { se = "Saved new instance of " ++ gm
                , ve = "Saved " ++ show (length new) ++ " features", gcErr = []})
        _ -> do
            saveGD gd "new_game"
            let new = findNew gd []
            return (GCEffect { se = "Saved new GameData."
            , ve = "Saved " ++ show (length new) ++ " features."
            , gcErr = [MissingFeatureError GameName] })
    where
        saveGD :: [(Feature, [CDSLExpr])] -> String -> IO ()
        saveGD cd n = do
            let filtered = filter (\(f, _) -> f /= Saved && f /= GameName) cd
            let conv = map (\(fs, er) -> (fs, unlines (map fromCDSLToString er))) filtered
            withFile (gameFolder ++ n ++ gameExtension) WriteMode $ \h -> do
                mapM_ (\(f, s) -> do
                        hPrint h f
                        hPutStrLn h s
                        hPutStrLn h "END"
                    ) conv


findNew :: Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
findNew [] _ = []
findNew _ [] = []
findNew old (y:ys) = case lookup (fst y) old of
    Just _ -> findNew old ys
    Nothing -> y : findNew old ys

findDiff :: Eq a => Eq b => [(a, b)] -> [(a, b)] -> [(a, b)]
findDiff [] _ = []
findDiff _ [] = []
findDiff old (y:ys) = case lookup (fst y) old of
    Just o -> if o == snd y
        then
            findDiff old ys
        else
            y : findDiff old ys
    Nothing -> findDiff old ys

findSim :: Eq a => Eq b => [(a, b)] -> [(a, b)] -> [(a, b)]
findSim [] _ = []
findSim _ [] = []
findSim old (y:ys) = case lookup (fst y) old of
    Just o -> if o == snd y
        then
            findSim old ys
        else
            y : findSim old ys
    Nothing -> findSim old ys