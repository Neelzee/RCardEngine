module CDSLExprTest.CDSLLoaderSPec where


import Test.Hspec ( describe, it, shouldBe, hspec )
import LoadGD (loadGameData')
import GD (GameData)
import PlayerMove (Move(PlayCard, DrawCard, Pass))
import Feature (Feature(CardSuits, CardValues, WinCon, EndCon, CardConstraints, PlayerMoves, PlayerHand, AnyTime, StartTime, CardRanks))
import CDSLExpr
    ( CDSLParseError,
      CDSLExpr(Text, Numeric, Greatest, Players, Score, Any, IsEmpty,
               Hand, IsEqual, CardRank, Deck, Shuffle, Always, If, Pile,
               Take, Swap),
      CDSLExpr(PlayerAction) )


test :: IO ()
test = hspec $ do
    describe "module.function" $ do
        it "loading file CardSL.cdsl" $ do
            rawC <- readFile "test/CDSLExprTest/CardDSL.cdsl"
            let c = lines rawC
            loadGameData' [] c `shouldBe` gameData



gameData :: Either GameData [CDSLParseError]
gameData = Left [
    (CardSuits, [Text "Hearts", Text "Diamonds", Text "Clubs", Text "Spades"])
    , (CardRanks, [Text "Ace", Text "2", Text "3", Text "4", Text "5", Text "6", Text "7", Text "8", Text "9", Text "10", Text "Jack", Text "Queen", Text "King"])
    , (CardValues, [Text "1", Text "2", Text "3", Text "4", Text "5", Text "6", Text "7", Text "8", Text "9", Text "10", Text "11", Text "12", Text "13"])
    , (WinCon, [Greatest (Players Score)])
    , (EndCon, [Any (Players (IsEmpty Hand)), Any (Players (IsEqual Score (Numeric 10)))])
    , (CardConstraints, [CardRank])
    , (PlayerMoves, [PlayerAction PlayCard False, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction Pass False])
    , (PlayerHand, [Numeric 4])
    , (AnyTime, [If [IsEmpty Deck] [Swap Pile Deck, Shuffle Deck, Take (Numeric 1) Deck Pile]])
    , (StartTime, [If [Always] [Shuffle Deck]])
    ]