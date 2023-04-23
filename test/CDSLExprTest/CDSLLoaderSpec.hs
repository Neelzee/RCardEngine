module CDSLExprTest.CDSLLoaderSPec where


import Test.Hspec ( describe, it, shouldBe, hspec, Spec )
import GameData.GD ( GameData )
import CDSL.CDSLExpr
import Feature
import CardGame.PlayerMove
import CardGame.Card
import GameData.LoadGD


test :: IO ()
test = do
    hspec $ do
        describe "GameData.LoadGD" $ do
            it "loading file CardSL.cdsl" $ do
                rawC <- readFile "games/CardDSL.cdsl"
                let c = lines rawC
                loadGameData' [] c `shouldBe` gameData
    


gameData :: Either GameData [CDSLParseError]
gameData = Left [
 
    ]
