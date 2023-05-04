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
        sortToAttributes = map (second (check . Map.toList))
            where
                check :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> [((Feature, Maybe [CDSLExpr]), [CDSLExpr])]
                check [] = []
                check (((GameName, _), _):xs) = check xs
                check (x:xs) = x : check xs


        showContent :: [((Feature, Maybe [CDSLExpr]), [CDSLExpr])] -> String
        showContent [] = ""
        showContent fs = intercalate "\n" (map (uncurry showFeature) fs)

        showFeature :: (Feature, Maybe [CDSLExpr]) -> [CDSLExpr] -> String
        showFeature (IgnoreConstraints, _) xs = show IgnoreConstraints ++ " = " ++ intercalate ", " (map go xs) ++ ";"
        showFeature (f, Nothing) xs = show f ++ " = [" ++ intercalate ", " (map go xs) ++ "];"
        showFeature (f, Just arg) xs = show f ++ " " ++ intercalate ", " (map fromCDSLToString arg)  ++ " = [" ++ intercalate "," (map go xs) ++ "];"

        go :: CDSLExpr -> String
        go (CEffect _ cs) = prettyShowCards cs
        go y = fromCDSLToString y
