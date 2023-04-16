import qualified CDSLExprTest.CDSLParseExprSpec as CDSLParseExprSpec (test)
import qualified CDSLExprTest.CDSLParseIfExprSpec as CDSLParseIfExprSpec (test)
import qualified CDSLExprTest.CDSLParseListSpec as CDSLParseListSpec (test)
import qualified CDSLExprTest.CDSLValidationSpec as CDSLValidationSpec (test)
import qualified CDSLExprTest.CDSLParsePlayerActionSpec as CDSLParsePlayerActionSpec (test)
import qualified CDSLExprTest.CDSLLoaderSPec as CDSLLoaderSPec (test)
import qualified FunctionsSpec as FunctionsSpec (test)



main :: IO ()
main = do
    FunctionsSpec.test
    CDSLParseExprSpec.test
    CDSLValidationSpec.test
    CDSLParseIfExprSpec.test
    CDSLParseListSpec.test
    CDSLParsePlayerActionSpec.test
    CDSLLoaderSPec.test