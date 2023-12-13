import Data.List.Split (splitOn)
import Data.List (transpose, find)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

main = do
    input <- map lines . splitOn "\n\n" <$> readFile "inputs/13.txt"
    print $ sum $ map getReflectionValue input
    print $ sum $ map getSmudgeReflectionValue input

getReflectionValue :: [String] -> Int
getReflectionValue s = fromMaybe rowReflection colReflection
        where cols = transpose s
              colReflection = getReflectionIndex cols
              rowReflection = 100 * fromJust (getReflectionIndex s)

getSmudgeReflectionValue :: [String] -> Int
getSmudgeReflectionValue s = fromMaybe rowReflection colReflection
    where cols = transpose s
          colReflection = getSmudgeReflectionIndex cols
          rowReflection = 100 * fromJust (getSmudgeReflectionIndex s)

getReflectionIndex :: [String] -> Maybe Int
getReflectionIndex s = (\(i, _, _) -> i) <$> find (\(i, bef, aft) -> bef == aft && isReflective i s) (zip3 [1..] s $ tail s)

getSmudgeReflectionIndex :: [String] -> Maybe Int
getSmudgeReflectionIndex s = (\(i, _, _) -> i) <$> find (\(i, bef, aft) -> countDifferences bef aft <= 1 && getReflectiveDifferences i s == 1) (zip3 [1..] s $ tail s)

isReflective :: Int -> [String] -> Bool
isReflective i l = all (uncurry (==)) $ zip to from
    where to = drop i l
          from = reverse $ take i l

getReflectiveDifferences :: Int -> [String] -> Int
getReflectiveDifferences i l = sum $ zipWith countDifferences to from
    where to = drop i l
          from = reverse $ take i l

countDifferences :: String -> String -> Int
countDifferences x y = length $ filter (uncurry (/=)) $ zip x y
