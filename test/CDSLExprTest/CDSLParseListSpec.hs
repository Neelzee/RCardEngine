module CDSLExprTest.CDSLParseListSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import ParseCardDSL (parseStringList)
import CDSLExpr


moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


exprTest' :: String -> String
exprTest' s = "should not parse '" ++ s ++ "'"


test :: IO ()
test = hspec $ do
    testParseCDSLFromStringSBV "Hearts, Diamonds, Clubs, Spades" [Text "Hearts", Text "Diamonds", Text "Clubs", Text "Spades"]
    testParseCDSLFromStringSBV "Hearts" [Text "Hearts"]
    testParseCDSLFromStringSBV "Ace, Two, Three" [Text "Ace", Text "Two", Text "Three"]
    testParseCDSLFromStringSBV "shuffle any" [Text "shuffle any"]




testParseCDSLFromStringSBV :: String -> [CDSLExpr] -> Spec
testParseCDSLFromStringSBV input result = describe (moduleName "parseIFCDSLFromString") $ do
    it (exprTest input) $ do
        parseStringList input `shouldBe` result