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
                let c = rawC
                loadGameData' [] c `shouldBe` (Left gameData)
    


gameData :: GameData
gameData = [
    (WinCon, [Greatest (Players Score)])
    , (EndCon,[Any (Players (IsEmpty Hand)), Any (Players (IsEqual Score (Numeric 10)))])
    , (TurnOrder,[TORight])
    , (CardSuits,[Text "Hearts",Text "Diamonds",Text "Clubs",Text "Spades"])
    , (CardValues,[Numeric 1,Numeric 2,Numeric 3,Numeric 4,Numeric 5,Numeric 6,Numeric 7,Numeric 8,Numeric 9,Numeric 10,Numeric 11,Numeric 12,Numeric 13])
    , (CardRanks,[Text "Ace",Text "Two",Text "Three",Text "Four",Text "Five",Text "Six",Text "Seven",Text "Eight",Text "Nine",Text "Ten",Text "Jack",Text "Queen",Text "King"])
    , (CardConstraints,[CardRank])
    , (ExceptionConstraints,[CEq,CardRank])
    , (IgnoreConstraints,[Cards [Card "Hearts" "Ace" 0]])
    , (CardCompare,[CGRq])
    , (CardEffects,[CEffect (ChangeCard [CardSuit]) [Card "Hearts" "Ace" 0]])
    , (CardEffects,[CEffect SwapHand [Card "Diamonds" "Ace" 0, Card "Spades" "Ace" 0]])
    , (CardEffects,[CEffect TakeFromHand [Card "Clubs" "Ace" 0]])
    , (CardEffects,[CEffect GiveCard [Card "Hearts" "Two" 0]])
    , (CardEffects,[CEffect PassNext [Card "Hearts" "King" 0]])
    , (CardEffects,[CEffect (DrawCards 2) [Card "Hearts" "Queen" 0, Card "Spades" "Queen" 0]])
    , (PlayerHand,[Numeric 4])
    , (PlayerMoves,[PlayerAction PlayCard False,PlayerAction DrawCard True,PlayerAction DrawCard True,PlayerAction DrawCard True,PlayerAction Pass False,PlayerAction DiscardCard False])
    , (AnyTime,[If [IsEmpty Deck] [Swap Pile Deck,Shuffle Deck,Take (Numeric 1) Deck Pile],If [IsEmpty Pile] [Take (Numeric 1) Deck Pile]])
    , (StartTime,[If [Always] [Shuffle Deck],If [IsEmpty Pile] [Take (Numeric 1) Deck Pile],If [Always] [Reset (CurrentPlayer PMoves)]])
    , (TurnEndTime,[If [Always] [Reset (CurrentPlayer PMoves)],If [PreviousPlayer (IsMove PAPass)] [GoBack Turn,Put Pile (CurrentPlayer Hand),GoForward Turn]])]
