module CDSLExprTest.CDSLParseExprSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSL.ParseCardDSL
import CDSL.CDSLExpr
import CDSL.CDSLExpr (CDSLExpr)
import Feature
import CardGame.PlayerMove


moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

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


testReadCDSLValid :: String -> (Feature, [CDSLExpr]) -> Spec
testReadCDSLValid inp res = describe (moduleName "readCDSL") $ do
    it (exprTest inp) $ do
        readCDSL inp `shouldBe` Left res
