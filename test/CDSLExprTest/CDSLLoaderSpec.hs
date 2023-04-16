module CDSLExprTest.CDSLLoaderSPec where


import Test.Hspec ( describe, it, shouldBe, hspec )
import GameData.LoadGD (loadGameData')
import GameData.GD (GameData)
import CDSL.CDSLExpr (CDSLParseError, CDSLExpr (Greatest, Players, Score, Any, IsEmpty, Hand, IsEqual, Numeric, Text, If, Deck, Pile, Shuffle, Take, Always, CardRank, Swap, CardValue, PlayerAction, CEffect))
import Feature (Feature(GameFeatures, CardFeatures, Actions, PlayerFeatures, WinCon, EndCon, CardValues, CardRanks, CardConstraints, PlayerMoves, PlayerHand, AnyTime, StartTime, CardSuits, CardEffects))
import CardGame.PlayerMove (Move(PlayCard, DrawCard, Pass))
import CardGame.Card (CardEffect(ChangeCard, TakeFromHand, SwapHand, GiveCard, PassNext, DrawCards), Card (..))


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
    , (CardRanks, [Text "Ace", Text "Two", Text "Three", Text "Four", Text "Five", Text "Six", Text "Seven", Text "Eight", Text "Nine", Text "Ten", Text "Jack", Text "Queen", Text "King"])
    , (CardConstraints, [CardRank])
    , (PlayerHand, [Numeric 4])
    , (PlayerMoves, [PlayerAction PlayCard False, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction Pass False])
    , (AnyTime, [If [IsEmpty Deck] [Swap Pile Deck, Shuffle Deck, Take (Numeric 1) Deck Pile], If [IsEmpty Pile] [Take (Numeric 1) Deck Pile]])
    , (StartTime, [If [Always] [Shuffle Deck], If [Always] [Take (Numeric 1) Deck Pile]])
    , (CardEffects, [CEffect ChangeCard [Card "Hearts" "Ace" 0]])
    , (CardEffects, [CEffect SwapHand [Card "Diamonds" "Ace" 0,Card "Spades" "Ace" 0]])
    , (CardEffects, [CEffect TakeFromHand [Card "Clubs" "Ace" 0]])
    , (CardEffects, [CEffect GiveCard [Card "Hearts" "Two" 0]])
    , (CardEffects, [CEffect PassNext [Card "Hearts" "King" 0]])
    , (CardEffects, [CEffect (DrawCards 2) [Card "Hearts" "Queen" 0, Card "Spades" "Queen" 0]])
    ]
