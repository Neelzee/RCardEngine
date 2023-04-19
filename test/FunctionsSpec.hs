module FunctionsSpec where


import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import Functions
import GameData.LoadGD (removeComments)

moduleName :: String -> String
moduleName s = "Functions." ++ s


test :: IO ()
test = hspec $ do
    -- lookupAll
    testLookupAll 1 [(0, "zero")] []
    testLookupAll 0 [(0, "zero")] ["zero"]
    testLookupAll 0 [(0, "zero"), (0, "zero")] ["zero", "zero"]

    -- trim
    testTrim "x" "x"
    testTrim " x" "x"
    testTrim " x " "x"
    testTrim " [x] " "[x]"
    testTrim " [ x ] " "[ x ]"

    -- splitAndTrim
    testSplitAndTrim "" [""]
    testSplitAndTrim " , " ["", ""]
    testSplitAndTrim "a,b" ["a", "b"]
    testSplitAndTrim "a ,b" ["a", "b"]
    testSplitAndTrim "a , b" ["a", "b"]
    testSplitAndTrim " a , b" ["a", "b"]
    testSplitAndTrim " a , b " ["a", "b"]

    -- takeUntilDuplicate
    testTakeUntilDuplicate "aa" "a"
    testTakeUntilDuplicate "abc" "abc"
    testTakeUntilDuplicate "abca" "abc"

    -- removeNth
    testRemoveNth 3 "abcdef" ('d', "abcef")
    testRemoveNth 2 "abcdef" ('c', "abdef")
    testRemoveNth 1 "abcdef" ('b', "acdef")
    testRemoveNth 0 "abcdef" ('a', "bcdef")

    -- removeFirst
    testRemoveFirst "abc" 'b' "ac"
    testRemoveFirst "abc" 'd' "abc"
    testRemoveFirst "abc" 'a' "bc"

    -- unique
    testUnique "aabbcc" "abc"
    testUnique "abc" "abc"
    testUnique "" ""

    -- lookUpOrDefault
    testLookUpOrDefault 0 "zero" [(1, "one")] "zero"
    testLookUpOrDefault 1 "zero" [(1, "one"), (2, "two"), (0, "zero")] "one"
    testLookUpOrDefault 2 "two" [] "two"

    -- mergeList
    testMergeList [(1, "one")] [(2, "two")] [(1, "one"), (2, "two")]
    testMergeList [(1, "one")] [(1, "two")] [(1, "one")]
    testMergeList [(1, "one")] [(1, "two"), (2, "two")] [(1, "one"), (2, "two")]

    -- stringToLists
    testStringToList
        "[abcd = [dadsad = [da, ad], ad = [adsam, ads]], ab = [ad]]"
        ["abcd = [dadsad = [da, ad], ad = [adsam, ads]]", "ab = [ad]"]
    testStringToList
        "[dadsad = [da, ad], ad = [adsam, ads]]"
        ["dadsad = [da, ad]", "ad = [adsam, ads]"]
    testStringToList
        "[[isEmpty deck] : [swap pile deck, shuffle deck, take 1 deck pile], isEmpty pile : [take 1 deck pile]]"
        ["[isEmpty deck] : [swap pile deck, shuffle deck, take 1 deck pile]", "isEmpty pile : [take 1 deck pile]"]
    testStringToList
        "[swap pile deck, shuffle deck, take 1 deck pile]"
        ["swap pile deck", "shuffle deck", "take 1 deck pile"]
    testStringToList "[any always]" ["any always"]
    testStringToList "[any always, any always]" ["any always", "any always"]
    testStringToList
        " [isEmpty deck : [swap pile deck, shuffle deck, take 1 deck pile], isEmpty pile : take 1 deck pile]"
        ["isEmpty deck : [swap pile deck, shuffle deck, take 1 deck pile]", "isEmpty pile : take 1 deck pile"]



testStringToList :: String -> [String] -> Spec
testStringToList inp res = describe (moduleName "stringToList") $ do
    it ("Input: " ++ inp ++ " Result: " ++ show res) $ do
        stringToList inp `shouldBe` res

testMergeList :: (Show a, Eq a, Show b, Eq b) => [(a, b)] -> [(a, b)] -> [(a, b)] -> Spec
testMergeList lstA lstB res = describe (moduleName "mergeList") $ do
    it ("List A: " ++ show lstA ++ " List B: " ++ show lstB ++ " Result: " ++ show res) $ do
        mergeList lstA lstB `shouldBe` res

testLookUpOrDefault :: (Show a, Eq a, Show k, Eq k) => k -> a -> [(k, a)] -> a -> Spec
testLookUpOrDefault k def lst res = describe (moduleName "lookupOrDefault") $ do
    it ("Key: " ++ show k ++ " Default: " ++ show def ++ " List: " ++ show lst ++ " Result: " ++ show res) $ do
        lookupOrDefault k def lst `shouldBe` res

testUnique :: (Show a, Eq a) => [a] -> [a] -> Spec
testUnique inp res = describe (moduleName "unique") $ do
    it ("Input: " ++ show inp ++ " Result: " ++ show res) $ do
        unique inp `shouldBe` res

testRemoveFirst :: (Show a, Eq a) => [a] -> a -> [a] -> Spec
testRemoveFirst lst el res = describe (moduleName "removeFirst") $ do
    it ("Input: " ++ show el ++ ", " ++ show lst ++ " Result: " ++ show res) $ do
        removeFirst lst el `shouldBe` res

testRemoveNth :: (Show a, Eq a) => Int -> [a] -> (a, [a]) -> Spec
testRemoveNth ind inp res = describe (moduleName "removeNth") $ do
    it ("Input: " ++ show inp ++ ", " ++ show ind ++ " Result: " ++ show res) $ do
        removeNth ind inp `shouldBe` res

testTakeUntilDuplicate :: (Show a, Eq a) => [a] -> [a] -> Spec
testTakeUntilDuplicate inp res = describe (moduleName "takeUntilDuplicate") $ do
    it ("Input: " ++ show inp ++ " Result: " ++ show res) $ do
        takeUntilDuplicate inp `shouldBe` res


testSplitAndTrim :: String -> [String] -> Spec
testSplitAndTrim inp res = describe (moduleName "splitAndTrim") $ do
    it ("Input: " ++ inp ++ " Result: " ++ show res) $ do
        splitAndTrim inp `shouldBe` res


testTrim :: String -> String -> Spec
testTrim inp res = describe (moduleName "trim") $ do
    it ("Input: " ++ inp ++ " Result: " ++ res) $ do
        trim inp `shouldBe` res


testLookupAll :: (Show a, Eq a, Show b, Eq b) => a -> [(a, b)] -> [b] -> Spec
testLookupAll key lst res = describe (moduleName "lookupAll") $ do
    it ("Key: " ++ show key ++ " List: " ++ show lst) $ do
        lookupAll key lst `shouldBe` res