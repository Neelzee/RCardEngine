module CDSLExprTest.CDSLValidationSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSL.CDSLExpr
import CDSL.CDSLValidater (validateCDSLExpression)
import CDSL.CDSLExpr (CDSLExecErrorCode(InvalidBoolEvaluationError), CDSLExecError (CDSLExecError), CDSLExpr (Shuffle))


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
    testParseCDSLFromStringSBV (If [IsEmpty Deck] [Swap Pile Deck, Shuffle Deck, Take (Numeric 1) Deck Pile])

    testParseCDSLFromStringSBE Score [(CDSLExecError { err = InvalidSyntaxError, expr = Score})]
    testParseCDSLFromStringSBE (Shuffle Always) [(CDSLExecError { err = InvalidSyntaxError, expr = Shuffle Always})]
    testParseCDSLFromStringSBE (Shuffle Never) [(CDSLExecError { err = InvalidSyntaxError, expr = Shuffle Never})]
    testParseCDSLFromStringSBE (Players Never) [(CDSLExecError { err = InvalidSyntaxError, expr = Players Never})]
    testParseCDSLFromStringSBE (If [Shuffle Deck] [Shuffle Pile])
        [CDSLExecError { err = InvalidBoolEvaluationError, expr = If [Shuffle Deck] [Shuffle Pile]}
        , CDSLExecError { err = InvalidBoolEvaluationError, expr = Shuffle Deck }]

testParseCDSLFromStringSBV :: CDSLExpr -> Spec
testParseCDSLFromStringSBV input = describe (moduleName "validateCDSLExpression") $ do
    it (exprTest input) $ do
        validateCDSLExpression input `shouldBe` Left input


testParseCDSLFromStringSBE :: CDSLExpr -> [CDSLExecError] -> Spec
testParseCDSLFromStringSBE input e = describe (moduleName "validateCDSLExpression") $ do
    it (exprTest' input) $ do
        validateCDSLExpression input `shouldBe` Right e
