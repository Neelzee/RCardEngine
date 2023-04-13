module CDSLExprTest.CDSLLoaderSPec where


import Test.Hspec ( describe, it, shouldBe, hspec )
import GameData.LoadGD (loadGameData')
import GameData.GD (GameData)
import CDSL.CDSLExpr (CDSLParseError, CDSLExpr (Greatest, Players, Score, Any, IsEmpty, Hand, IsEqual, Numeric, Text, If, Deck, Pile, Shuffle, Take, Always, CardRank, Swap, CardValue, PlayerAction))
import Feature (Feature(GameFeatures, CardFeatures, Actions, PlayerFeatures, WinCon, EndCon, CardValues, CardRanks, CardConstraints, PlayerMoves, PlayerHand, AnyTime, StartTime, CardSuits))
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass))


test :: IO ()
test = hspec $ do
    describe "module.function" $ do
        it "loading file CardSL.cdsl" $ do
            rawC <- readFile "test/CDSLExprTest/CardDSL.cdsl"
            let c = lines rawC
            loadGameData' [] c `shouldBe` gameData



gameData :: Either GameData [CDSLParseError]
gameData = Left [
        (WinCon, [Greatest (Players Score)])
        , (EndCon, [Any (Players (IsEmpty Hand)), Any (Players (IsEqual Score (Numeric 10)))])
        , (CardSuits, [Text "Hearts", Text "Diamonds", Text "Clubs", Text "Spades"])
        , (CardValues, [Numeric 1, Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10, Numeric 11, Numeric 12, Numeric 13])
        , (CardRanks, [Text "Ace", Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10, Text "Jack", Text "Queen", Text "King"])
        , (CardConstraints, [CardRank])
        , (PlayerMoves, [PlayerAction PlayCard False, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction Pass False])
        , (PlayerHand, [Numeric 4])
        , (AnyTime, [If [IsEmpty Deck] [Swap Pile Deck, Take (Numeric 1) Deck Pile, Shuffle Deck]])
        , (StartTime, [If [Always] [Shuffle Deck], If [Always] [Take (Numeric 1) Deck Pile]])
    ]