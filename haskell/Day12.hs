import Data.List.Split (splitOn)
import Data.List (group, intercalate)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as M

type Memo = (String, [Int])

main = do
    input <- map parseLine . lines <$> readFile "inputs/12.txt"
    print $ sum $ map countArrangements input
    print $ sum $ map (countArrangements . unfold) input

countArrangements :: (String, [Int]) -> Int
countArrangements (arr, groups) = fst $ memoizedCountArrangements arr groups M.empty

memoizedCountArrangements :: String -> [Int] -> M.Map Memo Int -> (Int, M.Map Memo Int)
memoizedCountArrangements [] [] mem = (1, mem) -- no springs and no groups left
memoizedCountArrangements [] _ mem = (0, mem) -- remaining groups but no springs
memoizedCountArrangements rest [] mem
    | '#' `elem` rest = (0, mem) -- no groups left but there are still damaged springs
    | otherwise = (1, mem) -- no groups and no damaged springs left
memoizedCountArrangements ('.':xs) groups mem = memoizedCountArrangements xs groups mem
memoizedCountArrangements rest@('#':xs) groups@(g:gs) mem
    | not (isArrangementValid curr g) = (0, mem) -- invalid arrangement
    | null nextRest && null gs = (1, mem) -- valid arrangement and no more groups to check
    | null nextRest && not (null gs) = (0, mem) -- valid arrangement but no springs left
    | head nextRest == '#' = (0, mem) -- no separator between current and next group
    | otherwise = memoizedCountArrangements (tail nextRest) gs mem -- continue with the remaining groups
    where curr = take g rest
          nextRest = drop g rest
memoizedCountArrangements rest@('?':xs) groups mem
    | isJust memoizedRes = (fromJust memoizedRes, mem)
    | otherwise = (result, M.insert key result hashMem)
    where key = (rest, groups)
          memoizedRes = key `M.lookup` mem
          (dotResult, dotMem) = memoizedCountArrangements ('.' : xs) groups mem
          (wildResult, hashMem) = memoizedCountArrangements ('#' : xs) groups dotMem
          result = dotResult + wildResult

isArrangementValid :: String -> Int -> Bool
isArrangementValid arr grp = grp == length (filter (/= '.') arr)

unfold :: Memo -> Memo
unfold (a, g) = (intercalate "?" $ rep a, concat $ rep g)
    where rep = replicate 5

parseLine :: String -> Memo
parseLine l = (springs, groups)
    where [springs, groupPart] = words l
          groups = map read $ splitOn "," groupPart
