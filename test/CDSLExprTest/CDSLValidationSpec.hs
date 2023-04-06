module CDSLExprTest.CDSLValidationSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSLExpr
import ParseCardDSL (validateCDSLExpression)


moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

exprTest :: CDSLExpr -> String
exprTest s = "should be valid '" ++ show s ++ "'"


exprTest' :: CDSLExpr -> String
exprTest' s = "should be invalid '" ++ show s ++ "'"

test :: IO ()
test = hspec $ do
    testParseCDSLFromStringSBV (Shuffle Deck)
    testParseCDSLFromStringSBV (Shuffle Pile)
    testParseCDSLFromStringSBV (Greatest (Players Score))
    testParseCDSLFromStringSBV Never

    testParseCDSLFromStringSBE Score (CDSLExecError { err = InvalidSyntaxError, expr = Score})
    testParseCDSLFromStringSBE (Shuffle Always) (CDSLExecError { err = InvalidSyntaxError, expr = Shuffle Always})
    testParseCDSLFromStringSBE (Shuffle Never) (CDSLExecError { err = InvalidSyntaxError, expr = Shuffle Never})
    testParseCDSLFromStringSBE (Players Never) (CDSLExecError { err = InvalidSyntaxError, expr = Players Never})
    testParseCDSLFromStringSBE (If [Shuffle Deck] [Shuffle Pile]) (CDSLExecError { err = InvalidSyntaxError, expr = If [Shuffle Deck] [Shuffle Pile]})

testParseCDSLFromStringSBV :: CDSLExpr -> Spec
testParseCDSLFromStringSBV input = describe (moduleName "validateCDSLExpression") $ do
    it (exprTest input) $ do
        validateCDSLExpression input `shouldBe` Left input


testParseCDSLFromStringSBE :: CDSLExpr -> CDSLExecError -> Spec
testParseCDSLFromStringSBE input e = describe (moduleName "validateCDSLExpression") $ do
    it (exprTest' input) $ do
        validateCDSLExpression input `shouldBe` Right e
