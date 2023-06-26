module CDSLExprTest.CDSLLoaderSpec where


import Test.Hspec ( describe, it, shouldBe, hspec, Spec )
import GameData.GD ( GameData )
import CDSL.CDSLExpr
import Feature
import CardGame.PlayerMove
import CardGame.Card
import GameData.LoadGD
import Data.Map

test :: IO ()
test = do
    hspec $ do
        describe "GameData.LoadGD" $ do
            it "loading file CardSL.cdsl" $ do
                rawC <- readFile "games/CardDSL.cdsl"
                let c = rawC
                let im = fromList [((GameName, Just []), [Text ""])]
                loadGameData' (fromList [(GameAttributes, im)]) c `shouldBe` (Left gameData)
    


gameData :: GameData
gameData = do
    let im = fromList [((GameName, Just []), [Text ""])]
    (fromList [(GameAttributes, im)])
