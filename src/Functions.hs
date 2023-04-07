module Functions where

import Data.List.Extra (splitOn)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Control.Monad ((>=>), join, forM)


-- Returns all Maybe lookups in a list
lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll x pairs = [b | (a, b) <- pairs, a == x]



-- Trim leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Split a string at the comma separator and trim each element
splitAndTrim :: String -> [String]
splitAndTrim = map trim . splitOn ","



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
removeFirst (x:xs) y = if x == y
    then
        xs
    else
        x : removeFirst xs y


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
mergeList [] ys = ys
mergeList ((xk, xv):xs) ys =
  case lookup xk ys of
    Just yv -> mergeList xs ((xk, yv):ys)
    Nothing -> mergeList xs ((xk, xv):ys)



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