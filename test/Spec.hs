import qualified CDSLExprTest.CDSLParserSpec as CDSLPrserSpec (test)
import qualified CDSLExprTest.CDSLValidationSpec as CDSLValidationSpec (test)
import qualified CDSLExprTest.CDSLLoaderSPec as CDSLLoaderSPec (test)




main :: IO ()
main = do
    CDSLPrserSpec.test
    CDSLValidationSpec.test
    CDSLLoaderSPec.test