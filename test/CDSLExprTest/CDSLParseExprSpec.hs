module CDSLExprTest.CDSLParseExprSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSL.ParseCardDSL
import CDSL.CDSLExpr
import CDSL.CDSLExpr (CDSLExpr)
import Feature


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

    -- Testing new parseCDSL
    testParseCDSLFError ["any", "always"] ([Any Always], [])
    testParseCDSLFError ["always", ":", "any", "never"] ([If [Always] [Any Never]], [])
    testParseCDSLFError ["always", ":", "always", ":", "never"] ([If [Always] [If [Always] [Never]]], [])

    testReadCDSL "card_suits = any always" (CardSuits, [Any Always])
    testReadCDSL "card_suits = [any always, any always]" (CardSuits, [Any Always, Any Always])
    testReadCDSL
        "card_values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]"
            (CardValues,
                [Numeric 1, Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10, Numeric 11, Numeric 12, Numeric 13])
    
    testReadCDSL
        "any_time = [isEmpty deck : [swap pile deck, shuffle deck, take 1 deck pile], isEmpty pile : take 1 deck pile]"
            (AnyTime, [If [IsEmpty Deck] [Swap Pile Deck, Take (Numeric 1) Deck Pile, Shuffle Deck]])


    testReadCDSL
        "card_ranks = [Ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King]"
            (CardRanks,
                [Text "Ace", Numeric 2, Numeric 3, Numeric 4, Numeric 5, Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10, Text "Jack", Text "Queen", Text "King"])
    
    testParseOneCDSL ["any", "always", "any", "always"] (Any Always, ["any", "always"])



    testParseCDSL ["any", "always", "any", "always"] ([Any Always, Any Always], [])


testParseCDSL :: [String] -> ([CDSLExpr], [CDSLParseError]) -> Spec
testParseCDSL input result = describe (moduleName "parseOneCDSL") $ do
    it (exprTest (show input)) $ do
        parseCDSLF input [] `shouldBe` result

testParseOneCDSL :: [String] -> (CDSLExpr, [String]) -> Spec
testParseOneCDSL input result = describe (moduleName "parseOneCDSL") $ do
    it (exprTest (show input)) $ do
        parseOneCDSL input 0 `shouldBe` Left result

testParseCDSLFromStringSBE :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromStringSBE input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest' input) $ do
        parseCDSLFromString (words input) `shouldBe` result


testParseCDSLFromStringSBC :: String -> Either CDSLExpr CDSLParseError -> Spec
testParseCDSLFromStringSBC input result = describe (moduleName "parseCDSLFromString") $ do
    it (exprTest input) $ do
        parseCDSLFromString (words input) `shouldBe` result



testParseCDSLFError :: [String] -> ([CDSLExpr], [CDSLParseError]) -> Spec
testParseCDSLFError input result = describe (moduleName "parseCDSLF") $ do
    it (exprTest (show input)) $ do
        parseCDSLF input [] `shouldBe` result




testReadCDSL :: String -> (Feature, [CDSLExpr]) -> Spec
testReadCDSL input result = describe (moduleName "readCDSL") $ do
    it (exprTest (show input)) $ do
        readCDSL input `shouldBe` Left result