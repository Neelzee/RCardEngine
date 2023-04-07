module CDSLExprTest.CDSLParseExprSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import ParseCardDSL (parseCDSLFromString)
import CDSLExpr


moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


exprTest' :: String -> String
exprTest' s = "should not parse '" ++ s ++ "'"

test :: IO ()
test = hspec $ do
    -- Should parse
    testParseCDSLFromStringSBC "greatest players score" (Left (Greatest (Players Score)))
    testParseCDSLFromStringSBC "any players isEmpty hand" (Left (Any (Players (IsEmpty Hand))))
    testParseCDSLFromStringSBC "any players isEqual score 10" (Left (Any (Players (IsEqual Score (Numeric 10)))))
    testParseCDSLFromStringSBC "any players isEqual 10 score" (Left (Any (Players (IsEqual (Numeric 10) Score))))
    testParseCDSLFromStringSBC "all players isEqual 10 score" (Left (All (Players (IsEqual (Numeric 10) Score))))
    testParseCDSLFromStringSBC "isEmpty deck" (Left (IsEmpty Deck))
    testParseCDSLFromStringSBC "isEmpty pile" (Left (IsEmpty Pile))
    testParseCDSLFromStringSBC "swap pile deck" (Left (Swap Pile Deck))
    testParseCDSLFromStringSBC "swap deck pile" (Left (Swap Deck Pile))
    testParseCDSLFromStringSBC "swap deck deck" (Left (Swap Deck Deck))
    testParseCDSLFromStringSBC "swap pile pile" (Left (Swap Pile Pile))
    testParseCDSLFromStringSBC "shuffle deck" (Left (Shuffle Deck))
    testParseCDSLFromStringSBC "shuffle pile" (Left (Shuffle Pile))
    testParseCDSLFromStringSBC "take 1 deck pile" (Left (Take (Numeric 1) Deck Pile))
    testParseCDSLFromStringSBC "take 1 pile deck" (Left (Take (Numeric 1) Pile Deck))
    testParseCDSLFromStringSBC "take 10 pile deck" (Left (Take (Numeric 10) Pile Deck))
    testParseCDSLFromStringSBC "take -1 pile deck" (Left (Take (Numeric (-1)) Pile Deck))
    testParseCDSLFromStringSBC "always" (Left Always)
    testParseCDSLFromStringSBC "never" (Left Never)
    testParseCDSLFromStringSBC "any always" (Left (Any Always))
    testParseCDSLFromStringSBC "1" (Left (Numeric 1))


    -- Should raise error
    testParseCDSLFromStringSBE "always : any never, any always" (Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Always, rawExpr = "always : any never, any always" }))
    testParseCDSLFromStringSBE "any" (Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Any Null, rawExpr = "any" }))
    testParseCDSLFromStringSBE "" (Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Null, rawExpr = "" }))
    testParseCDSLFromStringSBE "any take" (Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Any (Take Null Null Null), rawExpr = "any take" }))
    testParseCDSLFromStringSBE "any shuffle" (Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Any (Shuffle Null), rawExpr = "any shuffle" }))
    testParseCDSLFromStringSBE "any always never" (Right (CDSLParseError { pErr = UnnecessaryOperandError, pExpr = Any Always, rawExpr = "any always never" }))

testParseCDSLFromStringSBE :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromStringSBE input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest' input) $ do
        parseCDSLFromString (words input) `shouldBe` result


testParseCDSLFromStringSBC :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromStringSBC input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest input) $ do
        parseCDSLFromString (words input) `shouldBe` result
