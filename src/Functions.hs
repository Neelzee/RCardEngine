module Functions (
  lookupAll
  , trim
  , splitAndTrim
  , removeNth
  , mapCLCount
  , takeUntilDuplicate
  , deleteAt
  , removeFirst
  , updateAt
  , lookupOrDefault
  , subString
  , stringToList
  , mergeList
  , removeMaybe
  , allGames
  , lookupManyWithKey
  , count
  , dropFilteredCount
  , unique
  , removeLookup
  , removeLookupAll
  , remLst
  , elemLst
  ) where

import Data.List.Extra (splitOn, split)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory)
import Constants (gameFolder, gameExtension)
import Data.CircularList (CList, focus, update, isEmpty)



-- Returns all Maybe lookups in a list
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]



-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Split a string at the comma separator and trim each element
splitAndTrim :: String -> [String]
splitAndTrim  = map trim . splitOn ","



takeUntilDuplicate :: Eq a => [a] -> [a]
takeUntilDuplicate = go []
  where
    go _ [] = []
    go seen (y:ys)
      | y `elem` seen = []
      | otherwise = y : go (y:seen) ys


removeNth :: Int -> [a] -> (a, [a])
removeNth n xs = (xs !! n, take n xs ++ drop (n+1) xs)


-- Removes the first instance of the given element
removeFirst :: Eq a => [a] -> a -> [a]
removeFirst [] _ = []
removeFirst (x:xs) y
  | x == y = xs
  | otherwise = x : removeFirst xs y


-- Removes all duplicate elements from the given list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = if x `elem` xs
    then
        unique xs
    else
        x : unique xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lesser ++ [x] ++ quicksort greater
    where
        lesser = filter (< x) xs
        greater = filter (>= x) xs


lookupOrDefault :: Eq k => k -> a -> [(k, a)] -> a
lookupOrDefault key def assocList = fromMaybe def (lookup key assocList)




mergeList :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeList xs [] = xs
mergeList [] ys = ys
mergeList (x@(kX, _):xs) (y@(kY, _):ys)
  | kX == kY = x : mergeList xs ys
  | otherwise = x : y : mergeList xs ys


lookupManyWithKey :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupManyWithKey keys list = [(k, v) | (k, v) <- list, k `elem` keys]


removeMaybe :: [(Maybe a, b)] -> [(a, b)]
removeMaybe [] = []
removeMaybe ((x, y):xs) = case x of
    Just z -> (z, y) : removeMaybe xs
    Nothing -> removeMaybe xs


allGames :: IO [String]
allGames = do
  files <- listDirectory gameFolder
  return (map (uncurry (++)) $ filter (\(_, b) -> b == gameExtension) (map (span (/='.')) files))


count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys)
  | x == y = 1 + count x ys
  | otherwise = count x ys


dropFilteredCount :: Eq a => (a -> Bool) -> Int -> [a] -> [a]
dropFilteredCount _ _ [] = []
dropFilteredCount _ 0 ys = ys
dropFilteredCount f n (y:ys)
  | f y = dropFilteredCount f (n - 1) ys
  | otherwise = y : dropFilteredCount f n ys



mapCLCount :: CList a -> Int -> (a -> a) -> CList a
mapCLCount cs 0 _ = cs
mapCLCount cs n f
    | isEmpty cs = cs
    | otherwise = case focus cs of
        Just a -> mapCLCount (update (f a) cs) (n - 1) f
        Nothing -> mapCLCount cs n f




stringToList :: String -> [String]
stringToList s
    | hlst s && llst (reverse s) = do
        let res = reverse (map reverse (stringToList' (tail (init (trim s)))))
        map trim (rm res)
    | otherwise = [s]
    where
        rm :: [String] -> [String]
        rm [] = []
        rm (x:xs)
            | x == "" = rm xs
            | otherwise = x : rm xs

        hlst :: String -> Bool
        hlst [] = False
        hlst (y:ys)
            | y == ' ' = hlst ys
            | otherwise = '[' == y

        llst :: String -> Bool
        llst [] = False
        llst (y:ys)
            | y == ' ' = llst ys
            | otherwise = ']' == y

stringToList' :: String -> [String]
stringToList' "" = []
stringToList' xs = do
    let (acc, rems) = apd xs 0 "" []
    acc ++ stringToList' rems

    where
        apd :: String -> Int -> String -> [String] -> ([String], String)
        apd [] _ w wrds = (w:wrds, "")
        apd (y:ys) 0 w wrds = case y of
            '[' -> apd ys 1 (y:w) wrds
            ']' -> (wrds, ys)
            ',' -> apd ys 0 "" (w:wrds)
            z -> apd ys 0 (z:w) wrds
        apd (y:ys) n w wrds = case y of
            '[' -> apd ys (n + 1) (y:w) wrds
            ']' -> apd ys (n - 1) (y:w) wrds
            z -> apd ys n (z:w) wrds




deleteAt :: Int -> [a] -> [a]
deleteAt n xs = take n xs ++ drop (n+1) xs


updateAt :: Int -> a -> [a] -> [a]
updateAt n x xs = take n xs ++ [x] ++ drop (n+1) xs


removeLookup :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
removeLookup _ _ [] = []
removeLookup k v (x@(ok, _):xs)
  | k == ok = (k, v):xs
  | otherwise = x : removeLookup k v xs


removeLookupAll :: Eq a => a -> [b] -> [(a, b)] -> [(a, b)]
removeLookupAll _ _ [] = []
removeLookupAll k v (x@(ok, _):xs)
  | k == ok && null v = removeLookupAll k v xs
  | k == ok = map (\val -> (k, val)) v ++ removeLookupAll k [] xs
  | otherwise = x : removeLookupAll k v xs



subString :: String -> String -> Bool
subString "" _ = True
subString _ "" = False
subString (s:ss) (x:xs)
  | s == x = subString ss xs
  | otherwise = False


elemLst :: Eq a => [a] -> [a] -> Bool
elemLst [] _ = True
elemLst (x:xs) ys = x `elem` ys && elemLst xs ys

remLst :: [a] -> [Int] -> [a]
remLst xs ys = go xs (quicksort ys) 0
  where
    go :: [a] -> [Int] -> Int -> [a]
    go zs [] _ = zs
    go [] _ _ = []
    go (z:zs) (w:ws) n
      | w == n = go zs ws (n + 1)
      | otherwise = z : go zs (w:ws) (n + 1)