module CDSLExprTest.CDSLParserSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSLExpr (CDSLExpr(Greatest, Score, Players, IsEmpty, Hand, Any, Null, Deck, Pile, Swap, Shuffle, Take, Numeric, Always, Never, IsEqual), CDSLParseError (CDSLParseError, pErr, pExpr, rawExpr, IncompleteExpressionError))
import ParseCardDSL (parseCDSLFromString)


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


    -- Should raise error
    testParseCDSLFromStringSBE "any" (Right (CDSLParseError { pErr = IncompleteExpressionError, pExpr = Any Null, rawExpr = "any" }))

testParseCDSLFromStringSBE :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromStringSBE input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest' input) $ do
        parseCDSLFromString (words input) `shouldBe` result


testParseCDSLFromStringSBC :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromStringSBC input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest input) $ do
        parseCDSLFromString (words input) `shouldBe` result
