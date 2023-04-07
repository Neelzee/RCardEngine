module CDSLExprTest.CDSLParseIfExprSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import ParseCardDSL (parseIFCDSLFromString)
import CDSLExpr


moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


exprTest' :: String -> String
exprTest' s = "should not parse '" ++ s ++ "'"


test :: IO ()
test = hspec $ do
    testParseCDSLFromStringSBV "always : never" (If [Always] [Never])
    testParseCDSLFromStringSBV "always, always : never, never" (If [Always, Always] [Never, Never])
    testParseCDSLFromStringSBV "isEmpty deck : swap pile deck, shuffle deck, take 1 deck pile" (If [IsEmpty Deck] [Swap Pile Deck, Shuffle Deck, Take (Numeric 1) Deck Pile])




testParseCDSLFromStringSBV :: String -> CDSLExpr -> Spec
testParseCDSLFromStringSBV input result = describe (moduleName "parseIFCDSLFromString") $ do
    it (exprTest input) $ do
        parseIFCDSLFromString input `shouldBe` Left result