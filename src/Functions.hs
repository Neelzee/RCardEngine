module Functions where

import Data.List.Extra (splitOn)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Control.Monad ((>=>), join, forM)
import System.Directory (listDirectory)
import Constants (gameFolder)
import Data.CircularList (CList, focus, rotR, update, isEmpty, rotNL)



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


lookupMany :: Eq a => [a] -> [(a, b)] -> [b]
lookupMany keys pairs = mapMaybe (`lookup` pairs) keys

lookupManyWithKey :: Eq a => [a] -> [(a,b)] -> [(a,b)]
lookupManyWithKey keys list = [(k, v) | (k, v) <- list, k `elem` keys]

splitEithers :: [(a, [Either b c])] -> ([(a, [b])], [(a, [c])])
splitEithers [] = ([], [])
splitEithers ((a, x):xs) = bimap
  ((a, fst (partitionEithers x)) :)
  ((a, snd (partitionEithers x)) :) (splitEithers xs)

applyF :: (a -> [b] -> Maybe (a, [c])) -> [(a, [b])] -> Maybe [(a, [c])]
applyF f = traverse (\(a, bs) -> forM [bs] (f a)) >=> (pure . join)


removeMaybe :: [(Maybe a, b)] -> [(a, b)]
removeMaybe [] = []
removeMaybe ((x, y):xs) = case x of
    Just z -> (z, y) : removeMaybe xs
    Nothing -> removeMaybe xs


allGames :: IO [String]
allGames = listDirectory gameFolder


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



mapCLWhile :: CList a -> (a -> Bool) -> (a -> a) -> CList a
mapCLWhile cs prd f
    | isEmpty cs = cs
    | otherwise = case focus cs of
    Just _ -> go cs 0
    Nothing -> mapCLWhile (rotR cs) prd f
    where
        go clst n = case focus clst of
            Just y -> if prd y
                then
                    go (rotR (update (f y) cs)) (n + 1)
                else
                    rotNL n cs
            Nothing -> go (rotR cs) (n + 1)

mapCLCount :: CList a -> Int -> (a -> a) -> CList a
mapCLCount cs 0 _ = cs
mapCLCount cs n f
    | isEmpty cs = cs
    | otherwise = case focus cs of
        Just a -> mapCLCount (update (f a) cs) (n - 1) f
        Nothing -> mapCLCount cs n f

    


stringToList :: String -> [String]
stringToList s = do
    let res = reverse (map reverse (stringToList' (tail (init s))))
    map trim (rm res)
    where
        rm :: [String] -> [String]
        rm [] = []
        rm (x:xs)
            | x == "" = rm xs
            | otherwise = x : rm xs

stringToList' :: String -> [String]
stringToList' "" = []
stringToList' xs = do
    let (acc, rem) = apd xs 0 "" []
    acc ++ stringToList' rem

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


isList :: String -> Bool
isList xs = '[' `elem` xs && ']' `elem` xs && '=' `notElem` xs


mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf _ _ [] = []
mapIf prd f (x:xs)
  | prd x = f x : mapIf prd f xs
  | otherwise = x : mapIf prd f xs