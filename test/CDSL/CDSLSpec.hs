module CDSL.CDSLSpec where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import CDSL.CDSLLexer
import CDSL.CDSLAST


test :: IO () 
test = hspec $ do
  testBuildTreeValid [TypeToken Number, SymbolToken "foo", AssignToken, NumericToken 10] testInput 



testBuildTreeValid :: [CDSLLexer] -> [Node] -> Spec
testBuildTreeValid xs n = describe ("CDSL.CDSLAST") $ do
  it ("Input: " ++ show xs ++ " Result: " ++ show n) $ do
    buildTree xs `shouldBe` Just n




testInput :: [Node]
testInput =
  [
    (Node (TypeToken Number) Nothing (Just (
      (Node AssignToken Nothing (Just (
        (Node (SymbolToken "foo") Nothing (Just (
          (Node (NumericToken 10) Nothing (Just (
            (Node EndToken Nothing Nothing)
          )))
        )))
      )))
    )))
  ]
