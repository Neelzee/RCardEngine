module CDSLExprTest.CDSLParseExprSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSL.ParseCDSL
import CDSL.CDSLExpr
import CDSL.CDSLExpr (CDSLExpr (CurrentPlayer, PMoves, Always))
import Feature
import CardGame.PlayerMove
import Feature (Feature(TurnStartTime, TurnEndTime))
import CardGame.Card


moduleName :: String -> String
moduleName = ("ParseCDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


exprTest' :: String -> String
exprTest' s = "should not parse '" ++ s ++ "'"

test :: IO ()
test = hspec $ do
    testReadCDSLValid
        "card_suits = [Hearts, Diamonds, Clubs, Spades]"
            (CardSuits, [Text "Hearts", Text "Diamonds", Text "Clubs", Text "Spades"])

    testReadCDSLValid
        "card_constraints = rank"
            (CardConstraints, [CardRank])

    testReadCDSLValid
        "end_con = [any players isEmpty hand, any players isEqual score 10]"
            (EndCon, [Any (Players (IsEmpty Hand)), Any (Players (IsEqual Score (Numeric 10)))])
    

    testReadCDSLValid
        "start_time = [always : shuffle deck]"
            (StartTime, [If [Always] [Shuffle Deck]])

    testReadCDSLValid
        "start_time = [always : shuffle deck, always : take 1 deck pile]"
            (StartTime, [If [Always] [Shuffle Deck], If [Always] [Take (Numeric 1) Deck Pile]])

    testReadCDSLValid
        "any_time = [isEmpty deck : [swap pile deck, shuffle deck, take 1 deck pile], isEmpty pile : take 1 deck pile]"
            (AnyTime, [If [IsEmpty Deck] [Swap Pile Deck, Shuffle Deck, Take (Numeric 1) Deck Pile], If [IsEmpty Pile] [Take (Numeric 1) Deck Pile]])
    

    testReadCDSLValid
        "player_moves = [PLAY_CARD FALSE, DRAW_CARD TRUE, DRAW_CARD TRUE, DRAW_CARD TRUE, PASS FALSE]"
            (PlayerMoves, [PlayerAction PlayCard False, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction Pass False])

    testReadCDSLValid
        "draw_card 2 = [Hearts.Queen, Spades.Queen]"
            (CardEffects, [CEffect (DrawCards 2) [Card "Hearts" "Queen" 0, Card "Spades" "Queen" 0]])


    testReadCDSLValid
        "turn_start = [always : reset player moves]"
            (TurnStartTime, [If [Always] [Reset (CurrentPlayer PMoves)]])

    testReadCDSLValid
        "turn_end = [always : reset player moves]"
            (TurnEndTime, [If [Always] [Reset (CurrentPlayer PMoves)]])

    
    testReadCDSLValid
        "turn_end = [always : reset player moves, isSame rank look 4 pile : put pile discard, player isMove pPass : put pile player hand]"
            (TurnEndTime, [If [Always] [Reset (CurrentPlayer PMoves)], If [IsSame CardRank (Look (Numeric 4) Pile)] [Put Pile Discard], If [CurrentPlayer (IsMove PAPass)] [Put Pile (CurrentPlayer Hand)]])

            


testReadCDSLValid :: String -> (Feature, [CDSLExpr]) -> Spec
testReadCDSLValid inp res = describe (moduleName "readCDSL") $ do
    it (exprTest inp) $ do
        readCDSL inp `shouldBe` Left res
