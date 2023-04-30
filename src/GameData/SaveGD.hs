module GameData.SaveGD (saveGameData) where

import GameData.GD (GameData)
import Feature (Feature(..), Attribute, getAttribute)
import System.Directory (listDirectory)
import Constants (gameFolder, gameExtension)
import Data.List (elemIndex, intercalate)
import CDSL.CDSLExpr (CDSLExpr(..), CDSLParseErrorCode (ParseErrorOnLine), CDSLParseError (pErr), CardEffect (..))
import System.IO (withFile, IOMode (WriteMode), hPrint, hPutStrLn)
import CDSL.ExecCDSLExpr (fromCDSLToString)
import GameData.LoadGD (loadGameData)
import Terminal.GameCommands
import Functions (lookupAll)
import qualified Data.Map as Map
import CardGame.CardFunctions (prettyShowCards)

saveGameData :: GameData -> IO GCEffect
saveGameData gd = do
    let (_, gd') = span ((/=Saved) . fst) gd
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
                    Right (e, i') -> do
                        let gc = (GCEffect { se = "Overwrote old instance of " ++ gm
                            , ve = "Overwrote old instance, cannot compare differences due to an error on load."
                            , gcErr = [CDSLError (Right [e {pErr = ParseErrorOnLine (pErr e) i'}])]})
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
        saveGD :: GameData -> String -> IO ()
        saveGD cd n = do
            let filtered = filter (\(f, _) -> f /= Saved && f /= GameName) cd
            let pgd = Map.assocs $ Map.fromListWith (++) $ sortToAttributes filtered
            print pgd
            let cont = map (\(at, fs) -> show at ++ " {\n" ++ showContent fs ++ "\n}") pgd
            let cs = intercalate "\n\n" cont
            writeFile (gameFolder ++ "/" ++ n ++ gameExtension) cs

        sortToAttributes :: GameData -> [(Attribute, [(Feature, [CDSLExpr])])]
        sortToAttributes [] = []
        sortToAttributes (x@(f, _):xs) = case getAttribute f of
            at -> (at, [x]) : sortToAttributes xs


        showContent :: [(Feature, [CDSLExpr])] -> String
        showContent [] = ""
        showContent fs = intercalate "\n" (map (uncurry showFeature) fs)

        showFeature :: Feature -> [CDSLExpr] -> String
        showFeature CardEffects [] = ""
        showFeature CardEffects ((CEffect (ChangeCard _) crds):xs) = "change_card = [" ++ prettyShowCards crds ++ "];\n" ++ showFeature CardEffects xs
        showFeature CardEffects ((CEffect SwapHand crds):xs) = "swap_hand = [" ++ prettyShowCards crds ++ "];\n" ++ showFeature CardEffects xs
        showFeature CardEffects ((CEffect TakeFromHand crds):xs) = "take_from_hand = [" ++ prettyShowCards crds ++ "];\n" ++ showFeature CardEffects xs
        showFeature CardEffects ((CEffect PassNext crds):xs) = "pass_next = [" ++ prettyShowCards crds ++ "];\n" ++ showFeature CardEffects xs
        showFeature CardEffects ((CEffect (DrawCards i) crds):xs) = "draw_card " ++ show i ++ " = [" ++ prettyShowCards crds ++ "];\n" ++ showFeature CardEffects xs
        showFeature CardEffects ((CEffect GiveCard crds):xs) = "give_card = [" ++ prettyShowCards crds ++ "]" ++ "\n" ++ showFeature CardEffects xs
        showFeature ExceptionConstraints xs = show ExceptionConstraints ++ " " ++ fromCDSLToString (head xs) ++ " = [" ++ intercalate "," (map go xs) ++ "];"
        showFeature f xs = show f ++ " = [" ++ intercalate "," (map go xs) ++ "];"
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