module CDSLExprTest.CDSLValidationSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec, shouldThrow, anyException )
import CDSL.CDSLExpr
import CDSL.CDSLValidater (validateCDSLExpression)
import CDSL.CDSLExpr (CDSLExecErrorCode(InvalidBoolEvaluationError), CDSLExecError (CDSLExecError), CDSLExpr (Shuffle))
import Control.Exception

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

testParseCDSLFromStringSBV :: CDSLExpr -> Spec
testParseCDSLFromStringSBV input = describe (moduleName "validateCDSLExpression") $ do
    it (exprTest input) $ do
        validateCDSLExpression input `shouldBe` Left input

