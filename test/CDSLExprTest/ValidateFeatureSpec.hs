module CDSLExprTest.ValidateFeatureSpec where

    
import Test.Hspec ( describe, it, shouldBe, Spec, hspec )

import Feature (Feature (CEDrawCard))
import CDSL.ParseCardDSL (validateFeature)

moduleName :: String -> String
moduleName = ("ParseCardDSL." ++)

exprTest :: String -> String
exprTest s = "should parse '" ++ s ++ "'"


exprTest' :: String -> String
exprTest' s = "should not parse '" ++ s ++ "'"

test :: IO ()
test = hspec $ do
    testValidateFeatureValid "draw_card 2 " CEDrawCard


testValidateFeatureValid :: String -> Feature -> Spec
testValidateFeatureValid inp res = describe (moduleName "validateFeature") $ do
    it (exprTest inp) $ do
        validateFeature inp `shouldBe` Left res