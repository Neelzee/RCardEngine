module CDSLTokenizerSpec where


import Test.Hspec ( describe, it, shouldBe, hspec, Spec )
import CDSL.CDSLTokenizer
import CDSL.CDSLLexer
import CDSL.CDSLAST


test :: IO ()
test = do
  hspec $ do
    describe "CDSL.CDSLTokenizer" $ do
      it "Testing token input" $ do
        parseToTokens ["int", "foo", "=", "10", ";"] `shouldBe` [TypeToken Number, SymbolToken "foo", AssignToken, NumericToken 10, EndToken]
