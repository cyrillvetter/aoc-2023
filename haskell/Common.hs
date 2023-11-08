module Common where

import qualified Data.Set as S
import qualified Data.IntSet as IS

ints :: [String] -> [Int]
ints = map read

integers :: [String] -> [Integer]
integers = map read

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique list = length list == length (S.fromList list)

windowsOf :: Int -> [a] -> [[a]]
windowsOf size list
    | length list < size = []
    | otherwise = take size list : windowsOf size (drop 1 list)

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = check S.empty
    where
        check :: Ord a => S.Set a -> [a] -> a
        check set (x:xs)
            | x `S.member` set = x
            | otherwise = check (x `S.insert` set) xs

firstIntDuplicate :: [Int] -> Int
firstIntDuplicate = check IS.empty
    where
        check :: IS.IntSet -> [Int] -> Int
        check set (x:xs)
            | x `IS.member` set = x
            | otherwise = check (x `IS.insert` set) xs
