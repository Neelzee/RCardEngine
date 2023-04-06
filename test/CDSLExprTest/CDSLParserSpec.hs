module CDSLExprTest.CDSLParserSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSLExpr (CDSLExpr(Greatest, Score, Players, IsEmpty, Hand, Any), CDSLParseError)
import ParseCardDSL (parseCDSLFromString)


moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


test :: IO ()
test = hspec $ do
    testParseCDSLFromString "greatest players score" (Left (Greatest (Players Score)))
    testParseCDSLFromString "any players isEmpty hand" (Left (Any (Players (IsEmpty Hand))))
    



testParseCDSLFromString :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromString input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest input) $ do
        parseCDSLFromString (words input) `shouldBe` result
