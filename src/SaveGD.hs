module SaveGD where

import GD (GameData)
import Feature (Feature(..))
import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.List (elemIndex)
import CDSLExpr (CDSLExpr(..))
import LoadGD (loadFeatures)
import System.IO (withFile, IOMode (WriteMode), hPrint, hPutStrLn)
import ParseCardDSL (fromCDSLToString)
import Commands (CommandEffect (CommandEffect, short, verbose))


saveGameData :: GameData -> IO CommandEffect
saveGameData gd = do
    let (_, gd') = span ((/=Saved) . fst) (reverse gd)
    saveGameData' gd'

saveGameData' :: GameData -> IO CommandEffect
saveGameData' gd = do
    gl <- listDirectory gameFolder
    case lookup GameName gd of
        Just (Text gm:_) -> case elemIndex gm gl of
            Just i -> do
                res <- loadFeatures [] i
                case res of
                    Left oldGd -> do
                        let new = findNew oldGd gd
                        let diff = findDiff oldGd gd
                        let sim = findSim oldGd gd
                        saveGD gd gm
                        return (CommandEffect { short = "Overwrote old instance of " ++ gm,
                        verbose = "Overwrote " ++ show (length diff) ++ " features, added " ++ show (length new) ++ " features, " ++ show (length sim) ++ " features unchanged"})
                    Right e -> return (CommandEffect (show e) (show e))
            Nothing -> do
                saveGD gd gm
                let new = findNew [] gd
                return (CommandEffect { short = "Saved new instance of " ++ gm,
                verbose = "Saved " ++ show (length new) ++ " features"})
        _ -> do
            saveGD gd "new_game"
            let new = findNew gd []
            return (CommandEffect { short = "Saved new instance of " ++ "new game",
            verbose = "Saved " ++ show (length new) ++ " features"})
    where
        saveGD :: [(Feature, [CDSLExpr])] -> String -> IO ()
        saveGD cd n = do
            let filtered = filter (\(f, _) -> f /= Saved && f /= GameName) cd
            let conv = map (\(fs, er) -> (fs, unlines (map fromCDSLToString er))) filtered
            withFile ("games/" ++ n ++ ".txt") WriteMode $ \h -> do
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