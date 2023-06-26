import qualified CDSLExprTest.CDSLParseExprSpec as CDSLParseExprSpec (test)
import qualified CDSLExprTest.CDSLParseListSpec as CDSLParseListSpec (test)
import qualified CDSLExprTest.CDSLValidationSpec as CDSLValidationSpec (test)
import qualified CDSLExprTest.CDSLParsePlayerActionSpec as CDSLParsePlayerActionSpec (test)
import qualified CDSLExprTest.CDSLLoaderSpec as CDSLLoaderSpec (test)
import qualified FunctionsSpec (test)
import qualified CDSLExprTest.ValidateFeatureSpec as ValidateFeatureSpec (test)
--import qualified CDSL.CDSLSpec as CDSLSpec (test)
import qualified CDSLTokenizerSpec (test)

main :: IO ()
main = do
    --CDSLParseExprSpec.test
    --ValidateFeatureSpec.test
    --FunctionsSpec.test
    --CDSLValidationSpec.test
    --CDSLParseListSpec.test
    --CDSLParsePlayerActionSpec.test
    --CDSLLoaderSPec.test
    --CDSLSpec.test
    CDSLTokenizerSpec.test
