import qualified CDSLExprTest.CDSLParserSpec as CDSLPrserSpec (test)
import qualified CDSLExprTest.CDSLValidationSpec as CDSLValidationSpec (test)




main :: IO ()
main = do
    CDSLPrserSpec.test
    CDSLValidationSpec.test