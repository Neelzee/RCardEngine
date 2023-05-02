module GameData.SaveGD (saveGameData) where

import GameData.GD (GameData)
import Feature (Feature(..), Attribute (GameAttributes), getAttribute)
import System.Directory (listDirectory)
import Constants (gameFolder, gameExtension)
import Data.List (elemIndex, intercalate)
import CDSL.CDSLExpr (CDSLExpr(..), CDSLParseErrorCode (ParseErrorOnLine), CDSLParseError (pErr), CardEffect (..))
import System.IO (withFile, IOMode (WriteMode), hPrint, hPutStrLn)
import CDSL.ExecCDSLExpr (fromCDSLToString)
import GameData.LoadGD (loadGameData)
import Terminal.GameCommands
import Functions (lookupAll, lookupM)
import qualified Data.Map as Map
import CardGame.CardFunctions (prettyShowCards)
import CardGame.Card (Card)
import Data.Bifunctor (Bifunctor(second))


saveGameData :: GameData -> IO GCEffect
saveGameData gd = do
    gm <- case Map.lookup GameAttributes gd of
        Just att -> case lookupM GameName att of
            Just (_, Text gm:_) -> return gm
            _ -> return "new_game"
        _ -> return "new_game"
    saveGD gd gm
    return (GCEffect { se = "Saved " ++ gm
    , ve = "Saved " ++ show (Map.size gd) ++ " attributes, saved " ++ show (sum (map (\(_, b) -> Map.size b) (Map.toList gd))) ++ " features", gcErr = []})
    where
        saveGD :: GameData -> String -> IO ()
        saveGD cd n = do
            let pgd = Map.assocs $ Map.fromListWith (++) $ sortToAttributes (Map.toList cd)
            let cont = map (\(at, fs) -> show at ++ " {\n" ++ showContent fs ++ "\n}") pgd
            let cs = intercalate "\n\n" cont
            writeFile (gameFolder ++ "/" ++ n ++ gameExtension) cs


        sortToAttributes :: [(Attribute, Map.Map (Feature, Maybe [CDSLExpr]) [CDSLExpr])] -> [(Attribute, [((Feature, Maybe [CDSLExpr]), [CDSLExpr])])]
        sortToAttributes = map (second Map.toList)


        showContent :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> String
        showContent [] = ""
        showContent fs = intercalate "\n" (map (uncurry showFeature) fs)

        showFeature :: (Feature, Maybe [CDSLExpr]) -> [CDSLExpr] -> String
        showFeature (f, Nothing) xs = show f ++ " = [" ++ intercalate "," (map go xs) ++ "];"
        showFeature (f, Just arg) xs = show f ++ " " ++ intercalate ", " (map fromCDSLToString arg)  ++ " = [" ++ intercalate "," (map go xs) ++ "];"

        go :: CDSLExpr -> String
        go (CEffect ce _) = show ce
        go y = fromCDSLToString y

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