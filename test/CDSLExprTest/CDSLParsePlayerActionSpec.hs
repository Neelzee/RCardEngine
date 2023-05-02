module CDSLExprTest.CDSLParsePlayerActionSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSL.ParseCDSL (parseCDSLPlayerAction)
import CDSL.CDSLExpr
import CardGame.PlayerMove (Move(DrawCard, PlayCard, Pass))


moduleName :: String -> String
moduleName = ("ParseCDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


exprTest' :: String -> String
exprTest' s = "should not parse '" ++ s ++ "'"


test :: IO ()
test = hspec $ do
    testParseCDSLFromStringSBV "PLAYCARD FALSE" [PlayerAction PlayCard False]
    testParseCDSLFromStringSBV "PLAYCARD TRUE" [PlayerAction PlayCard True]
    testParseCDSLFromStringSBV "DRAWCARD FALSE" [PlayerAction DrawCard False]
    testParseCDSLFromStringSBV "PASS FALSE" [PlayerAction Pass False]
    testParseCDSLFromStringSBV "PLAYCARD FALSE, DRAWCARD TRUE, DRAWCARD TRUE, DRAWCARD TRUE, PASS FALSE" [PlayerAction PlayCard False, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction DrawCard True, PlayerAction Pass False]




testParseCDSLFromStringSBV :: String -> [CDSLExpr] -> Spec
testParseCDSLFromStringSBV input result = describe (moduleName "parseIFCDSLFromString") $ do
    it (exprTest input) $ do
        parseCDSLPlayerAction input `shouldBe` Left result